use tokio::fs;
use std::collections::HashMap;
use crate::db::invalid;
use crate::web::{Route, Response, BASE_DIR, Result, View, BaseRequest};

const SLASH: u8 = 47;
const LEFT_BRACE: u8 = 123;
const RIGHT_BRACE: u8 = 125;

pub type Routes<B> = Vec<(Vec<String>, View<B>)>;

pub struct Router<B: BaseRequest + 'static> {
    pub routes: Routes<B>,
    pub handle_404: View<B>,
    pub internal_error: Response,
    pub login_url: String,
    files: HashMap<&'static str, &'static [u8]>,
}

impl<B: BaseRequest> Router<B> {
    pub fn new(routes: Vec<Route<B>>, handle_404: View<B>, internal_error: Response, login_url: String, files: HashMap<&'static str, &'static [u8]>) -> Result<Self> {
        let mut router = Self {routes: vec![], handle_404, internal_error, login_url, files};
        let mut v = vec![];
        for route in routes {
            match route {
                Route::Path((url, f)) => {
                    v.push(((*url).to_string(), f));
                },
                Route::Import((url, r)) => {
                    for rt in r {
                        match rt {
                            Route::Path((u, f)) => {
                                v.push((format!("{}/{}", url, *u), *f));
                            },
                            _ => unimplemented!(),
                        }
                    }
                }
            }
        }
        for (url, f) in v {
            let cap = get_capture(&url)?;
            router.routes.push((cap, f));
        }
        Ok(router)
    }
    pub async fn serve_static(&self, url: &str) -> Result<Response> {
        let mut period = false;
        for c in url.chars() {
            if (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '-' || c == '/' || c == '_'  {
                if period {
                    period = false;
                }
                continue;
            } else if c == '.' {
                if !period {
                    period = true;
                    continue;
                }
            }
            return Err(invalid());
        }
        if let Some(f) = self.files.get(url) {
            return self.serve_content(url, f.to_vec());
        };
        let mut base = String::new();
        BASE_DIR.with(|b| base = b.clone());
        let full = format!("{}{}", base, url);
        let path = fs::canonicalize(&full).await?;
        if path.starts_with(base) {
            self.serve_content(url, fs::read(full).await?)
        } else {
            Err(invalid())
        }
    }
    fn serve_content(&self, url: &str, content: Vec<u8>) -> Result<Response> {
        let n = url.rfind('.').ok_or(invalid())?;
        let ty = match &url[n+1..] {
            "css" => "text/css",
            _ => return Err(invalid()),
        };
        Ok(Response::content("HTTP/1.1 200 OK", ty, content))
    }
}

pub fn get_capture(url: &str) -> Result<Vec<String>> {
    let mut routes = vec![];
    let mut n = 0;
    let mut iter = url.as_bytes().iter();
    if *iter.next().unwrap() != SLASH {
        return Err(invalid())
    }
    let mut m = 1;
    while m < url.len() {
        if let Some(c) = iter.next() {
            m += 1;
            if *c == LEFT_BRACE {
                n += 1;
                while let Some(d) = iter.next() {
                    if *d == RIGHT_BRACE {
                        break;
                    }
                    m += 1;
                }
                routes.push(String::from(&url[n..m]));
                m += 1;
                n = m;
            } else {
                while let Some(d) = iter.next() {
                    if *d == SLASH {
                        routes.push(String::from(&url[n..m]));
                        n = m;
                        m += 1;
                        break;
                    }
                    m += 1;
                }
            }
        } else {
            break;
        }
    }
    if n < m {
        routes.push(String::from(&url[n..]));
    }
    Ok(routes)
}

pub fn get_first(url: &str) -> String {
    match url.split_once('{') {
        Some((u, _)) => u.to_string(),
        None => url.to_string(),
    }
}

pub fn split_url<'a>(url: &'a str) -> Result<Vec<&'a str>> {
    let mut parts = vec![];
    let mut n = 0;
    let mut iter = url.as_bytes().iter();
    if *iter.next().unwrap() != SLASH {
        return Err(invalid())
    }
    let mut m = 1;
    while m < url.len() {
        while let Some(b) = iter.next() {
            if *b == SLASH {
                parts.push(&url[n..m]);
                n = m;
                m += 1;
                break;
            }
            m += 1;
        }
    }
    if n < m {
        parts.push(&url[n..]);
    }
    Ok(parts)
}
