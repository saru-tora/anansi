use std::{fmt, env, str, path::PathBuf};
use std::sync::{Arc, Mutex, RwLock};
use std::collections::HashMap;
use std::net::SocketAddr;
use tokio::net::{TcpSocket, TcpListener};
use tokio::io::AsyncWriteExt;
use tokio::sync::{Semaphore, oneshot::Sender, broadcast, mpsc};
use tokio::{fs, time, signal};
use std::pin::Pin;
use std::future::Future;

use toml;
use toml::value::Value;
use toml::map::Map;

use rand::seq::SliceRandom;
use rand::rngs::StdRng;
use rand::SeedableRng;
use sha2::{Digest, Sha256};

use http_body_util::Full;
use hyper::server::conn::http1;
use hyper::body::Bytes;
use hyper::{Request, body::Incoming as IncomingBody, Response as HyperResponse, header::HeaderValue};
use hyper::service::Service as HyperService;

use log::{error, info, warn};
use env_logger::Env;

use crate::db::{DbPool, DbRow, AsDb};
use crate::cache::BaseCache;
use crate::records::{VarChar, DateTime, DataType};
use crate::web::{BASE_DIR, Static, Route, BaseRequest, HyperRequest, Response, Http404, WebError, WebErrorKind, View, Service, route_request};
use crate::router::{RouteHandler, split_url};
use crate::migrations::{migrate, sql_migrate, make_migrations, AppMigration};
use crate::admin_site::AdminRef;
use crate::email::Mailer;

#[cfg(not(feature = "minimal"))]
use crate::web::{Result, route_path};

#[cfg(not(feature = "minimal"))]
use crate::router::get_capture;

pub static MAX_CONNECTIONS: usize = 1_000_000;

pub type Settings = Map<String, Value>;

type Timer = Arc<RwLock<String>>;

#[cfg(not(feature = "minimal"))]
#[macro_export]
macro_rules! main {
    () => {
        pub mod prelude {
            pub use async_trait::async_trait;
            pub use crate::project::Request;
            pub use anansi::{form, import, viewer, base_view, view, redirect, transact, form_error};
            pub use anansi::web::{Result, Response, BaseUser};
            pub use anansi::router::Router;
            pub use anansi::forms::Form;
            pub use anansi::cache::BaseCache;
            pub use anansi::records::Record;
            pub use anansi::site::Site;
        }

        #[tokio::main]
        async fn main() {
            use server_prelude::*;

            anansi::server::init_logger();
            let internal_error = || Response::internal_error(include_bytes!("http_errors/500.html").to_vec());
            let site = Arc::new(Mutex::new(anansi::util::admin::site::BasicAdminSite::new()));
            let app_data = crate::project::AppData::new().await;
            if let Some(server) = anansi::server::Server::new(app_data, APP_STATICS, APP_ADMINS, None, urls::routes, ErrorView::not_found, internal_error, app_services::<HttpRequest>, app_migrations, cmd::admin, site).await {
                server.run().await
            }
        }

        mod server_prelude {
            pub use std::sync::{Arc, Mutex};
            pub use crate::http_errors::views::ErrorView;
            pub use crate::project::{app_services, HttpRequest};
            pub use anansi::util::admin::site::AdminSite;
            pub use anansi::util::auth::cmd;
            pub use anansi::web::Response;
        }

        #[cfg(test)]
        pub async fn test_server(sender: tokio::sync::oneshot::Sender<()>) {
            use server_prelude::*;

            let internal_error = || Response::internal_error(include_bytes!("http_errors/500.html").to_vec());
            let site = Arc::new(Mutex::new(anansi::util::admin::site::BasicAdminSite::new()));
            anansi::server::Server::new(APP_STATICS, APP_ADMINS, Some(sender))
                .run(app_url, urls::ROUTES, ErrorView::not_found, internal_error, app_services::<HttpRequest>, app_migrations, cmd::admin, site)
                .await;
        }
    }
}

#[cfg(not(feature = "minimal"))]
pub struct Server<B: BaseRequest + 'static, C: BaseCache, D: AsDb, S: Service<B>> {
    listener: TcpListener,
    urls: Arc<HashMap<usize, Vec<String>>>,
    pub app_data: Arc<D>,
    pub cache: C,
    std_rng: Rng,
    router: Arc<RouteHandler<B, S>>,
    sem: Arc<Semaphore>,
    timer: Timer,
    site: AdminRef<B>,
    mailer: Option<Mailer>,
    notify_shutdown: broadcast::Sender<()>,
    shutdown_complete_tx: mpsc::Sender<()>,
    shutdown_complete_rx: mpsc::Receiver<()>,
}

#[cfg(feature = "minimal")]
pub struct Server<B: BaseRequest + 'static, C: BaseCache, D: anansi::db::AsDb, S: Service<B>> {
    listener: TcpListener,
    urls: Arc<HashMap<usize, Vec<String>>>,
    pub app_data: Arc<D>,
    pub cache: C,
    std_rng: Rng,
    router: Arc<RouteHandler<B, S>>,
    sem: Arc<Semaphore>,
    timer: Timer,
    mailer: Option<Mailer>,
    notify_shutdown: broadcast::Sender<()>,
    shutdown_complete_tx: mpsc::Sender<()>,
    shutdown_complete_rx: mpsc::Receiver<()>,
}

async fn get_pool<D: AsDb>(not_test: bool, settings: &Settings, migrations: fn() -> Vec<AppMigration<<D as AsDb>::SqlDb>>) -> <D as AsDb>::SqlDb {
    if not_test {
        match <D as AsDb>::SqlDb::new(&settings).await {
            Ok(p) => p,
            Err(_) => {
                println!("Trying to connect to the database again");
                let mut p = <D as AsDb>::SqlDb::new(&settings).await.expect("Database connection reattempt failed");
                if !cfg!(feature = "minimal") {
                    info!("starting migration");
                    migrate::<D>(migrations(), &mut p).await;
                    info!("migration successful");
                }
                p
            }
        }
    } else {
        let mut p = <D as AsDb>::SqlDb::test().await.expect("Database connection attempt failed");
        migrate::<D>(migrations(), &mut p).await;
        p
    }
}

pub type AdminInits<B> = &'static [fn(AdminRef<B>)];

pub fn init_logger() {
    env_logger::Builder::from_env(Env::default().default_filter_or("anansi_core")).init();
}

pub fn info_shutdown() {
    info!("Shutting down");
}

#[cfg(not(feature = "minimal"))]
impl<B: BaseRequest<SqlPool = D, Cache = C> + fmt::Debug + Clone, C: BaseCache + 'static, D: AsDb + 'static, S: Service<B> + 'static> Server<B, C, D, S> {
    pub async fn new(
        mut app_data: D,
        statics: &'static [&'static [Static]],
        admin_inits: AdminInits<B>,
        mut sender: Option<Sender<()>>,
        routes: fn() -> anansi::router::Router<B>,
        handle_404: View<B>,
        internal_error: fn() -> Response,
        services: fn(&Settings) -> std::pin::Pin<Box<dyn std::future::Future<Output = S> + Send + '_>>,
        migrations: fn() -> Vec<AppMigration<<D as AsDb>::SqlDb>>,
        admin: fn(<D as AsDb>::SqlDb) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>,
        site: AdminRef<B>) -> Option<Self>
        where <<<D as AsDb>::SqlDb as DbPool>::SqlRowVec as IntoIterator>::Item: DbRow
    {
        let args: Vec<String> = env::args().collect();

        let mut ip = None;

        let mut base = PathBuf::new();
        BASE_DIR.with(|b| base = b.clone());
        base.push("settings.toml");
        let s_settings = fs::read_to_string(&base).await.expect(&format!("Could not find {}", base.to_str().unwrap()));
        let settings: Settings = toml::from_str(&s_settings).expect("Could not parse settings.toml");

        if args.len() > 1 {
            match args[1].as_str() {
                "make-migrations" => {
                    if args.len() >= 2 {
                        make_migrations(&args[2], app_data.as_db()).await;
                    } else {
                        error!("expected app name");
                    }
                    return None;
                }
                "sql-migrate" => {
                    if args.len() >= 3 {
                        sql_migrate::<D>(migrations(), &args[2], &args[3]).await;
                    } else{
                        error!("expected app name");
                    }
                    return None;
                }
                "migrate" => {
                    migrate::<D>(migrations(), app_data.as_db_mut()).await;
                    return None;
                }
                "admin" => {
                    admin(app_data.as_db().clone()).await.expect("Could not create admin");
                    return None;
                }
                _ => ip = Some(args[1].to_string()),
            }
        }
        let addr = if let Some(s) = ip {
            if s.contains('.') {
                s
            } else {
                format!("127.0.0.1:{}", s)
            }
        } else {
            "127.0.0.1:9090".to_string()
        };
        let listener = reuse_listener(&addr);

        let mut url_map = HashMap::new();
        let mut files = HashMap::new();
        for stats in statics {
            for (name, file) in *stats {
                files.insert(*name, *file);
            }
        }
        let mut rv = routes().routes;
        app_url(&mut url_map, &rv);
        for admin_init in admin_inits {
            admin_init(site.clone());
        }
        for (name, view) in site.lock().unwrap().urls() {
            url_map.insert(*view as View<B> as usize, get_capture(name).unwrap());
            rv.push(route_path(name, *view));
        }
        let login_url = settings.get("login_url").expect("Could not get login url").as_str().expect("Expected string for login url").to_string();

        let router = Arc::new(RouteHandler::new(rv, handle_404, internal_error, login_url, services(&settings).await, files).unwrap());
        let urls = Arc::new(url_map);
        let c_settings = settings.get("caches").expect("Could not get cache settings").as_table().expect("Expected table for cache settings");
        let cache = C::new(&c_settings).await.expect("Could not start cache");
        let mailer = if let Ok(mailer) = Mailer::new(&settings) {
            Some(mailer)
        } else {
            None
        };

        let mut seed = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs().to_string();
        let std_rng = match settings.get("secret_key") {
            Some(key) => {
                seed += key.as_str().expect("Could not get secret");
                Rng::new(&seed)
            }
            None => {
                let rng = Rng::new(&seed);
                let s = format!("secret_key = \"{}\"\n{}", rng.secret_string(), s_settings);
                let mut file = fs::OpenOptions::new()
                  .write(true)
                  .open(&base)
                  .await
                  .expect(&format!("error with {}", base.to_str().unwrap()));
                file.write_all(&s.into_bytes()).await.unwrap();
                println!("Created new secret");
                rng
            }
        };
        let sem = Arc::new(Semaphore::new(MAX_CONNECTIONS));
        let timer = Arc::new(RwLock::new(DateTime::now().to_gmt()));

        if let Some(sender) = sender.take() {
            sender.send(()).unwrap();
        }

        let (notify_shutdown, _) = broadcast::channel(1);
        let (shutdown_complete_tx, shutdown_complete_rx) = mpsc::channel(1);

        Some(Self {listener, urls, app_data: Arc::new(app_data), cache, std_rng, router, sem, timer, site, mailer, notify_shutdown, shutdown_complete_tx, shutdown_complete_rx})
    }

    pub async fn run(mut self) {
        let shutdown_complete = self.shutdown_complete_tx.clone();
        let mut sd = Shutdown::new(self.notify_shutdown.subscribe());
        let tmr = self.timer.clone();

        tokio::spawn(async move {
            let _shutdown_complete = shutdown_complete;
            while !sd.is_shutdown() {
                tokio::select! {
                    _ = time::sleep(time::Duration::from_secs(1)) => {}
                    _ = sd.recv() => break,
                }
                let now = DateTime::now().to_gmt();
                *tmr.write().unwrap() = now;
            }
        });
        tokio::select! {
            _ = self.serve() => {}
            _ = signal::ctrl_c() => info!("Shutting down"),
        }

        let Server {notify_shutdown, shutdown_complete_tx, mut shutdown_complete_rx, ..} = self;
        drop(notify_shutdown);
        drop(shutdown_complete_tx);
        let _ = shutdown_complete_rx.recv().await;
    }
    async fn serve(&mut self) {
        loop {
            let urls = self.urls.clone();
            let app_data = self.app_data.clone();
            let cache = self.cache.clone();
            let std_rng = self.std_rng.clone();
            let router = self.router.clone();
            let sem = Arc::clone(&self.sem);
            let timer = Arc::clone(&self.timer);
            let aq = sem.try_acquire();
            let site = self.site.clone();
            let mailer = self.mailer.clone();
            let (stream, _) = self.listener.accept().await.unwrap();
            if aq.is_ok() {
                let mut sd = self.notify_shutdown.subscribe();
                let shutdown_complete = self.shutdown_complete_tx.clone();
                tokio::spawn(async move {
                    let _shutdown_complete = shutdown_complete;
                    let addr = stream.peer_addr().unwrap();
                    let svc = Svc { addr, urls, app_data, cache, std_rng, router, timer, site, mailer };

                    let res = tokio::select! {
                        res = http1::Builder::new()
                            .serve_connection(stream, svc) => res,
                        _ = sd.recv() => return,
                    };
                    if let Err(err) = res {
                        error!("Failed to serve connection: {}", err);
                    }
                });
            } else {
                error!("Open socket limit reached");
            }
        }
    }
}

pub fn app_url<B: BaseRequest>(hm: &mut std::collections::HashMap<usize, Vec<String>>, routes: &Vec<Route<B>>) {
    let mut v = vec![];
    for route in routes {
        match route {
            anansi::web::Route::Path((url, f)) => {
                v.push(((url).to_string(), *f));
            },
            anansi::web::Route::Import((url, r)) => {
                for rt in r {
                    match rt {
                        anansi::web::Route::Path((u, f)) => {
                            v.push((format!("{}/{}", url, u), *f));
                        },
                        _ => unimplemented!(),
                    }
                }
            }
        }
    }
    for (url, f) in v {
        let cap = anansi::router::get_capture(&url).unwrap();
        hm.insert(f as usize, cap);
    }
}

pub async fn get_db<D: AsDb>(migrations: fn() -> Vec<AppMigration<<D as AsDb>::SqlDb>>) -> <D as AsDb>::SqlDb {
    let mut base = PathBuf::new();
    BASE_DIR.with(|b| base = b.clone());
    base.push("settings.toml");
    let s_settings = fs::read_to_string(&base).await.expect(&format!("Could not find {}", base.to_str().unwrap()));
    let settings: Settings = toml::from_str(&s_settings).expect("Could not parse settings.toml");

    let not_test = true;
    get_pool::<D>(not_test, &settings, migrations).await

}

#[cfg(feature = "minimal")]
impl<B: BaseRequest<SqlPool = D, Cache = C> + fmt::Debug + Clone, C: BaseCache + 'static, D: anansi::db::AsDb + 'static + Clone, S: Service<B> + 'static> Server<B, C, D, S> {
    pub async fn new(
        mut app_data: D,
        statics: &'static [&'static [Static]],
        mut sender: Option<Sender<()>>,
        routes: fn() -> anansi::router::Router<B>,
        handle_404: View<B>,
        internal_error: fn() -> Response,
        services: fn(&Settings) -> std::pin::Pin<Box<dyn std::future::Future<Output = S> + Send + '_>>,
        migrations: fn() -> Vec<AppMigration<<D as AsDb>::SqlDb>>,
        last: bool) -> Option<Self>
        where <<<D as anansi::db::AsDb>::SqlDb as DbPool>::SqlRowVec as IntoIterator>::Item: DbRow
    {
        let args: Vec<String> = env::args().collect();

        let mut ip = None;

        let mut base = PathBuf::new();
        BASE_DIR.with(|b| base = b.clone());
        base.push("settings.toml");
        let s_settings = fs::read_to_string(&base).await.expect(&format!("Could not find {}", base.to_str().unwrap()));
        let settings: Settings = toml::from_str(&s_settings).expect("Could not parse settings.toml");

        if args.len() > 1 {
            match args[1].as_str() {
                "make-migrations" => {
                    if args.len() >= 2 {
                        make_migrations(&args[2], app_data.as_db()).await;
                    } else {
                        error!("expected app name");
                    }
                    return None;
                }
                "sql-migrate" => {
                    if args.len() >= 3 {
                        sql_migrate::<D>(migrations(), &args[2], &args[3]).await;
                    } else{
                        error!("expected app name");
                    }
                    return None;
                }
                "migrate" => {
                    migrate::<D>(migrations(), app_data.as_db_mut()).await;
                    return None;
                }
                _ => ip = Some(args[1].to_string()),
            }
        }
        let addr = if let Some(s) = ip {
            if s.contains('.') {
                s
            } else {
                format!("127.0.0.1:{}", s)
            }
        } else {
            "127.0.0.1:9090".to_string()
        };

        let listener = reuse_listener(&addr);

        let mut url_map = HashMap::new();
        let mut files = HashMap::new();
        for stats in statics {
            for (name, file) in *stats {
                files.insert(*name, *file);
            }
        }
        let rv = routes().routes;
        app_url(&mut url_map, &rv);
        let login_url = settings.get("login_url").expect("Could not get login url").as_str().expect("Expected string for login url").to_string();

        let router = Arc::new(RouteHandler::new(rv, handle_404, internal_error, login_url, services(&settings).await, files).unwrap());
        let urls = Arc::new(url_map);
        let c_settings = settings.get("caches").expect("Could not get cache settings").as_table().expect("Expected table for cache settings");
        let cache = C::new(&c_settings).await.expect("Could not start cache");
        let mailer = if let Ok(mailer) = Mailer::new(&settings) {
            Some(mailer)
        } else {
            None
        };

        let mut seed = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs().to_string();
        let std_rng = match settings.get("secret_key") {
            Some(key) => {
                seed += key.as_str().expect("Could not get secret");
                Rng::new(&seed)
            }
            None => {
                let rng = Rng::new(&seed);
                let s = format!("secret_key = \"{}\"\n{}", rng.secret_string(), s_settings);
                let mut file = fs::OpenOptions::new()
                  .write(true)
                  .open(&base)
                  .await
                  .expect(&format!("error with {}", base.to_str().unwrap()));
                file.write_all(&s.into_bytes()).await.unwrap();
                println!("Created new secret");
                rng
            }
        };
        let sem = Arc::new(Semaphore::new(MAX_CONNECTIONS));
        let timer = Arc::new(RwLock::new(DateTime::now().to_gmt()));

        if last {
            println!("Server running at http://{addr}/\nPress Ctrl+C to stop");
        }
        if let Some(sender) = sender.take() {
            sender.send(()).unwrap();
        }

        let (notify_shutdown, _) = broadcast::channel(1);
        let (shutdown_complete_tx, shutdown_complete_rx) = mpsc::channel(1);

        Some(Self {listener, urls, app_data: Arc::new(app_data), cache, std_rng, router, sem, timer, mailer, notify_shutdown, shutdown_complete_tx, shutdown_complete_rx})
    }

    pub async fn run(mut self) {
        let shutdown_complete = self.shutdown_complete_tx.clone();
        let mut sd = Shutdown::new(self.notify_shutdown.subscribe());
        let tmr = self.timer.clone();

        tokio::spawn(async move {
            let _shutdown_complete = shutdown_complete;
            while !sd.is_shutdown() {
                tokio::select! {
                    _ = time::sleep(time::Duration::from_secs(1)) => {}
                    _ = sd.recv() => break,
                }
                let now = DateTime::now().to_gmt();
                *tmr.write().unwrap() = now;
            }
        });

        tokio::select! {
            _ = self.serve() => {}
            _ = signal::ctrl_c() => {}
        }

        let Server {notify_shutdown, shutdown_complete_tx, mut shutdown_complete_rx, ..} = self;
        drop(notify_shutdown);
        drop(shutdown_complete_tx);
        let _ = shutdown_complete_rx.recv().await;
    }
    async fn serve(&mut self) {
        loop {
            let urls = self.urls.clone();
            let app_data = self.app_data.clone();
            let cache = self.cache.clone();
            let std_rng = self.std_rng.clone();
            let router = self.router.clone();
            let sem = Arc::clone(&self.sem);
            let timer = Arc::clone(&self.timer);
            let aq = sem.try_acquire();
            let mailer = self.mailer.clone();
            let (stream, _) = self.listener.accept().await.unwrap();
            
            if aq.is_ok() {
                let mut sd = self.notify_shutdown.subscribe();
                let shutdown_complete = self.shutdown_complete_tx.clone();
                tokio::spawn(async move {
                    let _shutdown_complete = shutdown_complete;
                    stream.set_nodelay(true).unwrap();
                    let addr = stream.peer_addr().unwrap();
                    let svc = Svc { addr, urls, app_data, cache, std_rng, router, timer, mailer };
                    let res = tokio::select! {
                        res = http1::Builder::new()
                            .serve_connection(stream, svc) => res,
                        _ = sd.recv() => return,
                    };
                    if let Err(err) = res {
                        error!("Failed to serve connection: {}", err);
                    }
                });
            } else {
                error!("Open socket limit reached");
            }
        }
    }
}

struct Shutdown {
    shutdown: bool,
    notify: broadcast::Receiver<()>,
}

impl Shutdown {
    fn new(notify: broadcast::Receiver<()>) -> Self {
        Self {shutdown: false, notify}
    }
    fn is_shutdown(&self) -> bool {
        self.shutdown
    }
    async fn recv(&mut self) {
        if self.shutdown {
            return;
        }

        let _ = self.notify.recv().await;

        self.shutdown = true;
    }
}

fn reuse_listener(addr: &String) -> TcpListener {
    let socket = TcpSocket::new_v4().unwrap();

    #[cfg(unix)]
    {
        if let Err(e) = socket.set_reuseport(true) {
            eprintln!("error setting SO_REUSEPORT: {}", e);
        }
    }

    socket.set_reuseaddr(true).unwrap();
    socket.bind(addr.parse().unwrap()).unwrap();

    socket.listen(1024).unwrap()
}

#[derive(Clone, Debug)]
pub struct Rng(Arc<Mutex<StdRng>>);

impl Rng {
    pub fn new(seed: &str) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(seed.as_bytes());
        let hash: [u8; 32] = hasher.finalize().as_slice().try_into().unwrap();
        Self {0: Arc::new(Mutex::new(StdRng::from_seed(hash)))}
    }

    pub fn secret_string(&self) -> String {
        let choices = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".as_bytes();
        let len = 32;
        let mut bytes = Vec::with_capacity(len);
        for _ in 0..len {
            bytes.push(*choices.choose(&mut *self.0.lock().unwrap()).unwrap());
        }
        String::from_utf8(bytes).unwrap()
    }

    pub fn new_secret(&self) -> VarChar<32> {
        VarChar::<32>::from_val(self.secret_string()).unwrap()
    }
}

pub async fn sleep(duration: std::time::Duration) {
    tokio::time::sleep(duration).await;
}

#[cfg(not(feature = "minimal"))]
struct Svc<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: AsDb, C: BaseCache, S: Service<B>> {
    addr: SocketAddr,
    urls: Arc<HashMap<usize, Vec<String>>>,
    app_data: Arc<D>,
    cache: C,
    std_rng: Rng, 
    router: Arc<RouteHandler<B, S>>,
    timer: Timer,
    site: AdminRef<B>,
    mailer: Option<Mailer>,
}

#[cfg(not(feature = "minimal"))]
impl<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: AsDb + 'static, C: BaseCache + 'static, S: Service<B> + 'static> HyperService<Request<IncomingBody>> for Svc<B, D, C, S> {
    type Response = HyperResponse<Full<Bytes>>;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = std::result::Result<Self::Response, Self::Error>> + Send>>;

    fn call(&mut self, request: Request<IncomingBody>) -> Self::Future {
        let urls = self.urls.clone();
        let app_data = self.app_data.clone();
        let cache = self.cache.clone();
        let std_rng = self.std_rng.clone();
        let site = self.site.clone();
        let router = self.router.clone();
        let timer = self.timer.clone();
        let mailer = self.mailer.clone();
        let req_info = if let Some(path_and_query) = request.uri().path_and_query() {
            format!("{} \"{} {} {:?}\"", self.addr, request.method(), path_and_query, request.version())
        } else {
            format!("{} \"{} {} {:?}\"", self.addr, request.method(), request.uri().path(), request.version())
        };

        let block = async move {
            let mut response = {
                let url = request.uri().clone();
                let dirs = if let Ok(dirs) = split_url(url.path()) {
                    dirs
                } else {
                    return Ok((router.internal_error)().into_inner());
                };
                if dirs[0] == "/static" {
                    match router.serve_static(url.path()).await {
                        Ok(r) => r,
                        Err(e) => {
                            error!("{}", e);
                            (router.internal_error)()
                        }
                    }
                } else {
                    let result = B::new(HyperRequest {0: request}, urls, app_data.clone(), cache.clone(), std_rng.clone(), site, mailer).await;
                    match result {
                        Ok(mut req) => {
                            match route_request(dirs, &mut req, &router.routes, &router.service).await {
                                Ok(res) => {
                                    res
                                }
                                Err(error) => {
                                    if let Some(web_error) = error.downcast_ref::<WebError>() {
                                        if web_error.kind() == &WebErrorKind::Unauthenticated {
                                            Response::redirect(&router.login_url)
                                        } else {
                                            error!("{}", web_error);
                                            (router.internal_error)()
                                        }
                                    } else if let Some(_) = error.downcast_ref::<Http404>() {
                                        match (router.handle_404)(&mut req).await {
                                            Ok(r) => r,
                                            Err(e) => {
                                                error!("{}", e);
                                                (router.internal_error)()
                                            }
                                        }
                                    } else {
                                        error!("{}", error);
                                        (router.internal_error)()
                                    }
                                }
                            }
                        }
                        Err(error) => {
                            if let Some(web_error) = error.downcast_ref::<WebError>() {
                                match web_error.kind() {
                                    WebErrorKind::NoSession => {
                                        match B::handle_no_session(Response::redirect(url.path()), app_data, std_rng).await {
                                            Ok(r) => {
                                                r
                                            }
                                            Err(e) => {
                                                error!("{}", e);
                                                (router.internal_error)()
                                            }
                                        }
                                    }
                                    WebErrorKind::Unauthenticated => {
                                        Response::redirect(&router.login_url)
                                    }
                                    _ => {
                                        error!("{}", web_error);
                                        (router.internal_error)()
                                    }
                                }
                            } else {
                                error!("{}", error);
                                (router.internal_error)()
                            }
                        }
                    }
                }
            };
            response.headers_mut().insert("Date", HeaderValue::from_str(&timer.read().unwrap()).unwrap());
            let status = response.status().as_u16();
            if status < 400 {
                info!("{} {}", req_info, status);
            } else if status < 500 {
                warn!("{} {}", req_info, status);
            } else {
                error!("{} {}", req_info, status);
            }
            Ok(response.into_inner())
        };

        Box::pin(block)
    }
}

#[cfg(feature = "minimal")]
struct Svc<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: AsDb, C: BaseCache, S: Service<B>> {
    addr: SocketAddr,
    urls: Arc<HashMap<usize, Vec<String>>>,
    app_data: Arc<D>,
    cache: C,
    std_rng: Rng, 
    router: Arc<RouteHandler<B, S>>,
    timer: Timer,
    mailer: Option<Mailer>,
}

#[cfg(feature = "minimal")]
impl<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: AsDb + 'static + Clone, C: BaseCache + 'static, S: Service<B> + 'static> HyperService<Request<IncomingBody>> for Svc<B, D, C, S> {
    type Response = HyperResponse<Full<Bytes>>;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = std::result::Result<Self::Response, Self::Error>> + Send>>;

    fn call(&mut self, request: Request<IncomingBody>) -> Self::Future {
        let urls = self.urls.clone();
        let app_data = self.app_data.clone();
        let cache = self.cache.clone();
        let std_rng = self.std_rng.clone();
        let router = self.router.clone();
        let timer = self.timer.clone();
        let mailer = self.mailer.clone();
        let req_info = if let Some(path_and_query) = request.uri().path_and_query() {
            format!("{} \"{} {} {:?}\"", self.addr, request.method(), path_and_query, request.version())
        } else {
            format!("{} \"{} {} {:?}\"", self.addr, request.method(), request.uri().path(), request.version())
        };

        let block = async move {
            let mut response = {
                let url = request.uri().clone();
                let dirs = if let Ok(dirs) = split_url(url.path()) {
                    dirs
                } else {
                    return Ok((router.internal_error)().into_inner());
                };
                if dirs[0] == "/static" {
                    match router.serve_static(url.path()).await {
                        Ok(r) => r,
                        Err(e) => {
                            error!("{}", e);
                            (router.internal_error)()
                        }
                    }
                } else {
                    let result = B::new(HyperRequest {0: request}, urls, app_data.clone(), cache.clone(), std_rng.clone(), mailer).await;
                    match result {
                        Ok(mut req) => {
                            match route_request(dirs, &mut req, &router.routes, &router.service).await {
                                Ok(res) => {
                                    res
                                }
                                Err(error) => {
                                    if let Some(web_error) = error.downcast_ref::<WebError>() {
                                        if web_error.kind() == &WebErrorKind::Unauthenticated {
                                            Response::redirect(&router.login_url)
                                        } else {
                                            error!("{}", web_error);
                                            (router.internal_error)()
                                        }
                                    } else if let Some(_) = error.downcast_ref::<Http404>() {
                                        match (router.handle_404)(&mut req).await {
                                            Ok(r) => r,
                                            Err(e) => {
                                                error!("{}", e);
                                                (router.internal_error)()
                                            }
                                        }
                                    } else {
                                        error!("{}", error);
                                        (router.internal_error)()
                                    }
                                }
                            }
                        }
                        Err(error) => {
                            if let Some(web_error) = error.downcast_ref::<WebError>() {
                                match web_error.kind() {
                                    WebErrorKind::NoSession => {
                                        match B::handle_no_session(Response::redirect(url.path()), app_data, std_rng).await {
                                            Ok(r) => {
                                                r
                                            }
                                            Err(e) => {
                                                error!("{}", e);
                                                (router.internal_error)()
                                            }
                                        }
                                    }
                                    WebErrorKind::Unauthenticated => {
                                        Response::redirect(&router.login_url)
                                    }
                                    _ => {
                                        error!("{}", web_error.kind());
                                        (router.internal_error)()
                                    }
                                }
                            } else {
                                error!("{}", error);
                                (router.internal_error)()
                            }
                        }
                    }
                }
            };
            response.headers_mut().insert("Date", HeaderValue::from_str(&timer.read().unwrap()).unwrap());
            let status = response.status().as_u16();
            if status < 400 {
                info!("{} {}", req_info, status);
            } else if status < 500 {
                warn!("{} {}", req_info, status);
            } else {
                error!("{} {}", req_info, status);
            }
            Ok(response.into_inner())
        };

        Box::pin(block)
    }
}
