use std::{fmt, env, str, path::PathBuf, marker::PhantomData};
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use tokio::net::{TcpListener, TcpStream};
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::sync::Semaphore;
use tokio::{fs, time};

use toml;
use toml::value::Value;
use toml::map::Map;

use rand::seq::SliceRandom;
use rand::rngs::StdRng;
use rand::SeedableRng;
use sha2::{Digest, Sha256};

use crate::db::{DbPool, DbRow};
use crate::cache::BaseCache;
use crate::records::{VarChar, DateTime, DataType};
use crate::web::{BASE_DIR, Result, Static, Route, BaseRequest, RawRequest, Response, Http404, WebError, WebErrorKind, View, route_request, route_path};
use crate::router::{Router, get_capture, split_url};
use crate::migrations::{migrate, sql_migrate, make_migrations, AppMigration};
use crate::admin_site::AdminRef;

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
        fn main() {
            use std::sync::{Arc, Mutex};
            use crate::urls::app_url;
            use crate::http_errors::views::ErrorView;
            use anansi::util::admin::site::AdminSite;
            use anansi::web::Response;
            let internal_error = Response::internal_error(include_bytes!("http_errors/500.html"));
            let site = Arc::new(Mutex::new(anansi::util::admin::site::BasicAdminSite::new()));
            anansi::server::Server::new(APP_STATICS, APP_ADMINS).run(app_url, urls::ROUTES, ErrorView::not_found, internal_error, app_migrations, anansi::util::auth::cmd::admin, site);
        }
    }
}

pub struct Server<B: BaseRequest + 'static, D: DbPool> {
    statics: &'static [&'static [Static]],
    admin_inits: AdminInits<B>,
    d: PhantomData<D>,
}

pub type AdminInits<B> = &'static [fn(AdminRef<B>)];

impl<B: BaseRequest<SqlPool = D, Cache = C> + fmt::Debug + Clone, C: BaseCache + 'static, D: DbPool + 'static> Server<B, D> {
    pub fn new(statics: &'static [&'static [Static]], admin_inits: AdminInits<B>) -> Self {
        Self {statics, admin_inits, d: PhantomData}
    }
    pub fn run(&mut self, url_mapper: fn(&mut HashMap<usize, Vec<String>>), routes: &[Route<B>], handle_404: View<B>, internal_error: Response, migrations: fn() -> Vec<AppMigration<D>>, admin: fn(D) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>, site: AdminRef<B>) where <<D as DbPool>::SqlRowVec as IntoIterator>::Item: DbRow {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let addr = "127.0.0.1:9090";
            let listener = TcpListener::bind(addr).await.unwrap();

            let args: Vec<String> = env::args().collect();

            let mut base = PathBuf::new();
            BASE_DIR.with(|b| base = b.clone());
            base.push("settings.toml");
            let s_settings = fs::read_to_string(&base).await.expect(&format!("Could not find {}", base.to_str().unwrap()));
            let settings: Settings = toml::from_str(&s_settings).expect("Could not parse settings.toml");

            let pool = match D::new(&settings).await {
                Ok(p) => p,
                Err(_) => {
                    let p = D::new(&settings).await.expect("Database connection reattempt failed");
                    migrate(migrations(), &p).await;
                    p
                }
            };

            if args.len() > 1 {
                match args[1].as_str() {
                    "make-migrations" => {
                        if args.len() >= 2 {
                            make_migrations(&args[2], &pool).await;
                        } else {
                            eprintln!("expected app name");
                        }
                    }
                    "sql-migrate" => {
                        if args.len() >= 3 {
                            sql_migrate(migrations(), &args[2], &args[3]).await;
                        } else{
                            eprintln!("expected app name");
                        }
                    }
                    "migrate" => {
                        migrate(migrations(), &pool).await;
                    }
                    "admin" => {
                        admin(pool.clone()).await.expect("Could not create admin");
                    }
                    _ => eprintln!("Unrecognized argument"),
                }
            } else {
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
                let router = Arc::new(Router::new(rv, handle_404, internal_error, login_url, files).unwrap());
                let urls = Arc::new(url_map);
                let c_settings = settings.get("caches").expect("Could not get cache settings").as_table().expect("Expected table for cache settings");
                let cache = C::new(&c_settings).await.expect("Could not start cache");

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
                    if aq.is_ok() {
                        tokio::spawn(async move {
                            handle_connection(stream, urls, pool, cache, std_rng, router, timer, site).await.unwrap();
                        });
                    } else {
                        eprintln!("Open socket limit reached");
                    }
                }
            }
        })
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

pub async fn handle_connection<B: BaseRequest<SqlPool = D, Cache = C> + 'static + fmt::Debug, D: DbPool, C: BaseCache>(mut stream: TcpStream, urls: Arc<HashMap<usize, Vec<String>>>, pool: D, cache: C, std_rng: Rng, router: Arc<Router<B>>, timer: Timer, site: AdminRef<B>) -> Result<()> {
    let mut buffer = vec![0; 1024];
    while let Ok(l) = time::timeout(time::Duration::from_secs(5), stream.read(&mut buffer)).await {
        let length = l.unwrap();
        if length == 0 {
            break;
        }
        let urls = urls.clone();
        let pool = pool.clone();
        let cache = cache.clone();
        let std_rng = std_rng.clone();
        let site = site.clone();

        let mut response = {
            let (n, request_line) = RawRequest::<D>::get_request_line(&buffer[..length])?;
            let url = request_line.url.clone();
            let dirs = split_url(&url)?;
            if dirs[0] == "/static" {
                match router.serve_static(&url).await {
                    Ok(r) => r,
                    Err(_) => router.internal_error.clone(),
                }
            } else {
                let result = B::new(&buffer[n..length], request_line, urls, pool.clone(), cache.clone(), std_rng.clone(), site).await;
                match result {
                    Ok(mut req) => {
                        match route_request(dirs, &mut req, &router.routes).await {
                            Ok(res) => {
                                res
                            }
                            Err(error) => {
                                if let Some(web_error) = error.downcast_ref::<WebError>() {
                                    if web_error.kind() == &WebErrorKind::Unauthenticated {
                                        Response::redirect(&router.login_url)
                                    } else {
                                        router.internal_error.clone()
                                    }
                                } else if let Ok(_http_404) = error.downcast::<Http404>() {
                                    match (router.handle_404)(&mut req).await {
                                        Ok(r) => r,
                                        Err(_) => router.internal_error.clone(),
                                    }
                                } else {
                                    router.internal_error.clone()
                                }
                            }
                        }
                    }
                    Err(error) => {
                        if let Ok(web_error) = error.downcast::<WebError>() {
                            match web_error.kind() {
                                WebErrorKind::NoSession => {
                                    match B::handle_no_session(Response::redirect(&url), pool, std_rng).await {
                                        Ok(r) => {
                                            r
                                        }
                                        Err(_) => {
                                            router.internal_error.clone()
                                        }
                                    }
                                }
                                WebErrorKind::Unauthenticated => {
                                    Response::redirect(&router.login_url)
                                }
                                WebErrorKind::Invalid => {
                                    router.internal_error.clone()
                                }
                            }
                        } else {
                            router.internal_error.clone()
                        }
                    }
                }
            }
        };
        {
            let timer = timer.lock().unwrap();
            response.headers_mut().insert("Date".to_string(), format!("{timer}"));
        }
        stream.write(&response.into_bytes()).await.unwrap();
        break;
    }
    Ok(())
}
