use std::{fmt, env, str, path::PathBuf, marker::PhantomData};
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::net::SocketAddr;
use tokio::net::TcpListener;
use tokio::io::AsyncWriteExt;
use tokio::sync::{Semaphore, oneshot::Sender};
use tokio::{fs, time};
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

use log::{error, info};
use env_logger::Env;

use crate::db::{DbPool, DbRow};
use crate::cache::BaseCache;
use crate::records::{VarChar, DateTime, DataType};
use crate::web::{BASE_DIR, Result, Static, Route, BaseRequest, HyperRequest, Response, Http404, WebError, WebErrorKind, View, Service, route_request, route_path};
use crate::router::{Router, get_capture, split_url};
use crate::migrations::{migrate, sql_migrate, make_migrations, AppMigration};
use crate::admin_site::AdminRef;
use crate::email::Mailer;

pub type Settings = Map<String, Value>;

type Timer = Arc<Mutex<DateTime>>;

#[macro_export]
macro_rules! main {
    () => {
        pub mod prelude {
            pub use async_trait::async_trait;
            pub use crate::project::Request;
            pub use anansi::{form, import, viewer, base_view, view, redirect, transact, form_error};
            pub use anansi::web::{Result, Response, BaseUser};
            pub use anansi::forms::Form;
            pub use anansi::cache::BaseCache;
            pub use anansi::records::Record;
            pub use anansi::util::auth::records::Group;
        }

        #[tokio::main]
        async fn main() {
            use server_prelude::*;

            let internal_error = || Response::internal_error(include_bytes!("http_errors/500.html").to_vec());
            let site = Arc::new(Mutex::new(anansi::util::admin::site::BasicAdminSite::new()));
            anansi::server::Server::new(APP_STATICS, APP_ADMINS, None)
                .run(app_url, urls::ROUTES, ErrorView::not_found, internal_error, app_services::<HttpRequest>, app_migrations, cmd::admin, site)
                .await;
        }

        mod server_prelude {
            pub use std::sync::{Arc, Mutex};
            pub use crate::urls::app_url;
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

pub struct Server<B: BaseRequest + 'static, D: DbPool> {
    statics: &'static [&'static [Static]],
    admin_inits: AdminInits<B>,
    d: PhantomData<D>,
    sender: Option<tokio::sync::oneshot::Sender<()>>,
}

async fn get_pool<D: DbPool>(not_test: bool, settings: &Settings, migrations: fn() -> Vec<AppMigration<D>>) -> D {
    if not_test {
        match D::new(&settings).await {
            Ok(p) => p,
            Err(_) => {
                let p = D::new(&settings).await.expect("Database connection reattempt failed");
                migrate(migrations(), &p).await;
                p
            }
        }
    } else {
        let p = D::test().await.expect("Database connection attempt failed");
        migrate(migrations(), &p).await;
        p
    }
}

pub type AdminInits<B> = &'static [fn(AdminRef<B>)];

impl<B: BaseRequest<SqlPool = D, Cache = C> + fmt::Debug + Clone, C: BaseCache + 'static, D: DbPool + 'static> Server<B, D> {
    pub fn new(statics: &'static [&'static [Static]], admin_inits: AdminInits<B>, sender: Option<Sender<()>>) -> Self {
        Self {statics, admin_inits, d: PhantomData, sender}
    }
    pub async fn run<S: Service<B> + 'static>(
        &mut self,
        url_mapper: fn(&mut HashMap<usize, Vec<String>>),
        routes: &'static [Route<B>],
        handle_404: View<B>,
        internal_error: fn() -> Response,
        services: fn(&Settings) -> std::pin::Pin<Box<dyn std::future::Future<Output = S> + Send + '_>>,
        migrations: fn() -> Vec<AppMigration<D>>,
        admin: fn(D) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>,
        site: AdminRef<B>)
        where <<D as DbPool>::SqlRowVec as IntoIterator>::Item: DbRow
    {
        env_logger::Builder::from_env(Env::default().default_filter_or("anansi_core")).init();

        let args: Vec<String> = env::args().collect();

        let mut ip = None;

        let mut base = PathBuf::new();
        BASE_DIR.with(|b| base = b.clone());
        base.push("settings.toml");
        let s_settings = fs::read_to_string(&base).await.expect(&format!("Could not find {}", base.to_str().unwrap()));
        let settings: Settings = toml::from_str(&s_settings).expect("Could not parse settings.toml");

        let not_test = self.sender.is_none();
        let pool = get_pool(not_test, &settings, migrations).await;

        if args.len() > 1 {
            match args[1].as_str() {
                "make-migrations" => {
                    if args.len() >= 2 {
                        make_migrations(&args[2], &pool).await;
                    } else {
                        error!("expected app name");
                    }
                    return;
                }
                "sql-migrate" => {
                    if args.len() >= 3 {
                        sql_migrate(migrations(), &args[2], &args[3]).await;
                    } else{
                        error!("expected app name");
                    }
                    return;
                }
                "migrate" => {
                    migrate(migrations(), &pool).await;
                    return;
                }
                "admin" => {
                    admin(pool.clone()).await.expect("Could not create admin");
                    return;
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
        let listener = TcpListener::bind(&addr).await.unwrap();

        let mut url_map = HashMap::new();
        url_mapper(&mut url_map);
        let mut files = HashMap::new();
        for stats in self.statics {
            for (name, file) in *stats {
                files.insert(*name, *file);
            }
        }
        let mut rv = routes.to_vec();
        for admin_init in self.admin_inits {
            admin_init(site.clone());
        }
        for (name, view) in site.lock().unwrap().urls() {
            url_map.insert(*view as View<B> as usize, get_capture(name).unwrap());
            rv.push(route_path(name, *view));
        }
        let login_url = settings.get("login_url").expect("Could not get login url").as_str().expect("Expected string for login url").to_string();

        let router = Arc::new(Router::new(rv, handle_404, internal_error, login_url, services(&settings).await, files).unwrap());
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
        let sem = Arc::new(Semaphore::new(10000000));
        let timer = Arc::new(Mutex::new(DateTime::now()));
        let t2 = Arc::clone(&timer);
        tokio::spawn(async move {
            loop {
                time::sleep(time::Duration::from_secs(1)).await;
                let mut t2 = t2.lock().unwrap();
                *t2 = DateTime::now();
            }
        });

        println!("Server running at http://{addr}/\nPress Ctrl+C to stop");
        if let Some(sender) = self.sender.take() {
            sender.send(()).unwrap();
        }

        loop {
            let (stream, _) = listener.accept().await.unwrap();
            let urls = urls.clone();
            let pool = pool.clone();
            let cache = cache.clone();
            let std_rng = std_rng.clone();
            let router = router.clone();
            let sem = Arc::clone(&sem);
            let timer = Arc::clone(&timer);
            let aq = sem.try_acquire();
            let site = site.clone();
            let mailer = mailer.clone();
            if aq.is_ok() {
                tokio::spawn(async move {
                    let addr = stream.peer_addr().unwrap();
                    if let Err(err) = http1::Builder::new()
                        .serve_connection(stream, Svc { addr, urls, pool, cache, std_rng, router, timer, site, mailer })
                        .await
                    {
                        error!("Failed to serve connection: {:?}", err);
                    }
                });
            } else {
                error!("Open socket limit reached");
            }
        }
    }
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

struct Svc<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: DbPool, C: BaseCache, S: Service<B>> {
    addr: SocketAddr,
    urls: Arc<HashMap<usize, Vec<String>>>,
    pool: D,
    cache: C,
    std_rng: Rng, 
    router: Arc<Router<B, S>>,
    timer: Timer,
    site: AdminRef<B>,
    mailer: Option<Mailer>,
}

impl<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: DbPool + 'static, C: BaseCache + 'static, S: Service<B> + 'static> HyperService<Request<IncomingBody>> for Svc<B, D, C, S> {
    type Response = HyperResponse<Full<Bytes>>;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = std::result::Result<Self::Response, Self::Error>> + Send>>;

    fn call(&mut self, request: Request<IncomingBody>) -> Self::Future {
        let urls = self.urls.clone();
        let pool = self.pool.clone();
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
                        Err(_) => (router.internal_error)(),
                    }
                } else {
                    let result = B::new(HyperRequest {0: request}, urls, pool.clone(), cache.clone(), std_rng.clone(), site, mailer).await;
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
                                            (router.internal_error)()
                                        }
                                    } else if let Ok(_http_404) = error.downcast::<Http404>() {
                                        match (router.handle_404)(&mut req).await {
                                            Ok(r) => r,
                                            Err(_) => (router.internal_error)()
                                        }
                                    } else {
                                        (router.internal_error)()
                                    }
                                }
                            }
                        }
                        Err(error) => {
                            if let Ok(web_error) = error.downcast::<WebError>() {
                                match web_error.kind() {
                                    WebErrorKind::NoSession => {
                                        match B::handle_no_session(Response::redirect(url.path()), pool, std_rng).await {
                                            Ok(r) => {
                                                r
                                            }
                                            Err(_) => {
                                                (router.internal_error)()
                                            }
                                        }
                                    }
                                    WebErrorKind::Unauthenticated => {
                                        Response::redirect(&router.login_url)
                                    }
                                    WebErrorKind::Invalid => {
                                        (router.internal_error)()
                                    }
                                }
                            } else {
                                (router.internal_error)()
                            }
                        }
                    }
                }
            };
            {
                let timer = timer.lock().unwrap();
                response.headers_mut().insert("Date", HeaderValue::from_str(&format!("{timer}")).unwrap());
            }
            info!("{} {}", req_info, response.status().as_u16());
            Ok(response.into_inner())
        };

        Box::pin(block)
    }
}
