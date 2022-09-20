use std::env;
use std::str;
use std::sync::{Arc, Mutex};
use std::fmt;
use std::collections::HashMap;
use tokio::net::{TcpListener, TcpStream};
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::sync::Semaphore;
use tokio::{fs, time};

use rand::seq::SliceRandom;
use rand::rngs::StdRng;
use rand::SeedableRng;
use sha2::{Digest, Sha256};

use crate::db::DbPool;
use crate::models::{VarChar, DateTime, DataType};
use crate::web::{BASE_DIR, Result, Static, Route, BaseRequest, RawRequest, Response, Http404, HttpError, HttpErrorKind, View, route_request, path};
use crate::router::{Router, get_capture, split_url};
use crate::syntax::{migrate, make_migrations, AppMigration};

type Timer = Arc<Mutex<DateTime>>;

#[macro_export]
macro_rules! main {
    () => {
        pub mod prelude {
            pub use async_trait::async_trait;
            pub use crate::settings::Request;
            pub use anansi::{form, import, viewer, view, redirect, transact, form_error};
            pub use anansi::web::{if_guest, Result, Response};
            pub use anansi::forms::Form;
            pub use anansi::models::Model;
        }
        fn main() {
            use crate::urls::app_url;
            use crate::http_errors::views::ErrorView;
            use anansi::web::Response;
            let internal_error = Response::internal_error(include_bytes!("http_errors/500.html"));
            anansi::server::Server::new(APP_STATICS, PAGES).run(app_url, urls::ROUTES, ErrorView::not_found, internal_error, APP_MIGRATIONS, anansi::util::auth::cmd::admin);
        }
    }
}

pub struct Server<B: BaseRequest + 'static> {
    statics: &'static [&'static [Static]],
    pages: Pages<B>,
}

pub type Pages<B> = &'static [fn(&mut Vec<(&'static str, View<B>)>)];

impl<B: BaseRequest + fmt::Debug + Clone> Server<B> {
    pub fn new(statics: &'static [&'static [Static]], pages: Pages<B>) -> Self {
        Self {statics, pages}
    }
    pub fn run(&mut self, url_mapper: fn(&mut HashMap<usize, Vec<String>>), routes: &[Route<B>], handle_404: View<B>, internal_error: Response, migrations: &[AppMigration], admin: fn(DbPool) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let addr = "127.0.0.1:9090";
            let listener = TcpListener::bind(addr).await.unwrap();

            let args: Vec<String> = env::args().collect();

            let pool = match DbPool::new().await {
                Ok(p) => p,
                Err(_) => {
                    let p = DbPool::new().await.expect("Database connection reattempt failed");
                    migrate(migrations, &p).await;
                    p
                },
            };

            if args.len() > 1 {
                match args[1].as_str() {
                    "make-migrations" => {
                        if args.len() >= 2 {
                            make_migrations(&args[2], &pool).await;
                        } else {
                            eprintln!("expected app name");
                        }
                    },
                    "migrate" => {
                        migrate(migrations, &pool).await;
                    },
                    "admin" => {
                        admin(pool.clone()).await.expect("Could not create admin");
                    },
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
                for page in self.pages {
                    let mut v = vec![];
                    page(&mut v);
                    for (name, view) in &v {
                        url_map.insert(*view as View<B> as usize, get_capture(name).unwrap());
                        rv.push(path(name, *view));
                    }
                }
                let router = Arc::new(Router::new(rv, handle_404, internal_error, files).unwrap());
                let urls = Arc::new(url_map);

                let mut base = String::new();
                BASE_DIR.with(|b| base = b.clone());
                let dir = format!("{}/secret", base);
                let mut seed = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs().to_string();
                let std_rng = match fs::read(&dir).await {
                    Ok(key) => {
                        seed += &String::from_utf8_lossy(&key);
                        Rng::new(&seed)
                    },
                    Err(_) => {
                        let rng = Rng::new(&seed);
                        let s = rng.secret_string();
                        fs::write(&dir, &s).await.unwrap();
                        println!("Created new secret");
                        rng
                    },
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
                    let std_rng = std_rng.clone();
                    let router = router.clone();
                    let sem = Arc::clone(&sem);
                    let timer = Arc::clone(&timer);
                    let aq = sem.try_acquire();
                    if aq.is_ok() {
                        tokio::spawn(async move {
                            handle_connection(stream, urls, pool, std_rng, router, timer).await.unwrap();
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

pub async fn handle_connection<B: BaseRequest + 'static + fmt::Debug>(mut stream: TcpStream, urls: Arc<HashMap<usize, Vec<String>>>, pool: DbPool, std_rng: Rng, router: Arc<Router<B>>, timer: Timer) -> Result<()> {
    let mut buffer = [0; 1024];
    let l = stream.read(&mut buffer).await.unwrap();

    let mut response = {
        let (n, request_line) = RawRequest::get_request_line(&buffer)?;
        let url = request_line.url.clone();
        let dirs = split_url(&url)?;
        if dirs[0] == "/static" {
            match router.serve_static(&url).await {
                Ok(r) => r,
                Err(_) => router.internal_error.clone(),
            }
        } else {
            let result = B::new(&buffer[n..l], request_line, urls, pool.clone(), std_rng.clone()).await;
            match result {
                Ok(req) => {
                    match route_request(dirs, req, &router.routes).await {
                        Ok(res) => {
                            res
                        },
                        Err(error) => {
                            if let Ok(http_404) = error.downcast::<Http404<B>>() {
                                match (router.handle_404)(http_404.req()).await {
                                    Ok(r) => r,
                                    Err(_) => router.internal_error.clone(),
                                }
                            } else {
                                router.internal_error.clone()
                            }
                        }
                    }
                },
                Err(error) => {
                    if let Ok(http_error) = error.downcast::<HttpError>() {
                        match http_error.kind() {
                            HttpErrorKind::NoSession => {
                                match B::handle_no_session(Response::redirect(&url), pool, std_rng).await {
                                    Ok(r) => {
                                        r
                                    },
                                    Err(_) => {
                                        router.internal_error.clone()
                                    },
                                }
                            },
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
    stream.flush().await.unwrap();
    Ok(())
}
