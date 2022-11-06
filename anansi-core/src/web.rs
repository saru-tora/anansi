use async_trait::async_trait;
use std::sync::Arc;
use std::collections::{HashMap, hash_map::Iter};
use crate::db::{DbPool, invalid};
use std::string::FromUtf8Error;
use std::error::Error;
use std::result;
use std::fmt;
use std::pin::Pin;
use std::future::Future;
use std::path::PathBuf;

use crate::cache::BaseCache;
use crate::server::Rng;
use crate::router::Routes;
use crate::records::{Record, FromParams, BigInt, DateTime};
use crate::admin_site::AdminRef;

pub type Result<T> = result::Result<T, Box<dyn Error + Send + Sync>>;
pub type View<B> = fn(&mut B) -> Pin<Box<dyn Future<Output = Result<Response>> + Send + '_>>;
pub type Static = (&'static str, &'static [u8]);

const SPACE: u8 = 32;
const AMPERSAND: u8 = 38;
const SEMICOLON: u8 = 59;
const EQUAL: u8 = 61;

pub const SLASH: u8 = 47;
pub const LEFT_BRACE: u8 = 123;

pub const GET: Method = Method::Get;
pub const POST: Method = Method::Post;

fn find_base(dir: &mut PathBuf) -> bool {
    let files = std::fs::read_dir(&dir).unwrap();
    for f in files {
        let f = f.unwrap();
        if f.file_type().unwrap().is_file() && f.file_name() == "Cargo.toml" {
            return true;
        }
    }
    if dir.pop() {
        if find_base(dir) {
            return true;
        }
    } else {
        return false;
    }
    panic!("Could not find Cargo.toml");
}


thread_local!{
    pub static BASE_DIR: PathBuf = {
        let cwd = std::env::current_dir().unwrap();
        let mut dir = cwd.clone();
        find_base(&mut dir);
        let mut orig = dir.clone();
        match std::fs::canonicalize(&dir) {
            Ok(_) => {
                dir
            }
            Err(_) => {
                let name = cwd.as_path().file_name().unwrap();
                orig.push(name);
                orig
            }
        }
    }
}

pub mod prelude {
    pub use anansi::{import, routes, path};
}

#[macro_export]
macro_rules! middleware {
    () => {
        #[derive(Debug, Clone)]
        pub struct HttpRequest {
            raw: anansi::web::RawRequest<Pool>,
            mid: Middleware,
            urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>,
            cache: AppCache,
            admin: anansi::util::admin::site::AdminRef<Self>,
        }
        #[derive(Debug, Clone)]
        pub struct Middleware {
            user: anansi::util::auth::records::User,
            session: anansi::util::sessions::records::Session,
            session_data: anansi::util::sessions::records::SessionData,
        }
        impl Middleware {
            pub async fn new(raw: &mut anansi::web::RawRequest<Pool>) -> $crate::web::Result<Self> {
                use anansi::records::{Record, DataType};
                let session = match anansi::util::sessions::records::Session::from_raw(raw).await {
                    Ok(s) => s,
                    Err(_) => return Err(Box::new(anansi::web::WebError::from(anansi::web::WebErrorKind::NoSession))),
                };
                let session_data = session.to_data()?;
                let user_id = session_data.get(anansi::util::auth::records::User::KEY)?.as_i64().expect("could not get user id");
                let user = if user_id == 0 {
                    anansi::util::auth::records::User::guest()
                } else {
                    anansi::util::auth::records::User::find(anansi::records::BigInt::from_val(user_id)?).raw_get(raw.pool()).await?
                };
                Ok(Middleware {user, session, session_data})
            }
        }
        impl anansi::web::BaseMiddleware for Middleware {}
        anansi::request_derive!();
        pub trait Request: anansi::web::BaseRequest + anansi::util::sessions::middleware::Sessions + anansi::util::auth::middleware::Auth + anansi::util::admin::site::HasAdmin + anansi::web::CsrfDefense  + anansi::web::Reverse + std::fmt::Debug + anansi::web::GetRecord {}
        impl Request for HttpRequest {}
        impl anansi::util::admin::site::HasAdmin for HttpRequest {
            fn admin(&self) -> anansi::util::admin::site::AdminRef<Self> {
                self.admin.clone()
            }
        }
        impl anansi::util::auth::admin::Request for HttpRequest {}
        #[async_trait::async_trait]
        impl anansi::util::auth::middleware::Auth for HttpRequest {}
        #[async_trait::async_trait]
        impl anansi::util::sessions::middleware::Sessions for HttpRequest {
            fn session(&self) -> &anansi::util::sessions::records::Session {
                &self.mid.session
            }
            fn session_data(&self) -> &anansi::util::sessions::records::SessionData {
                &self.mid.session_data
            }
            fn session_data_mut(&mut self) -> &mut anansi::util::sessions::records::SessionData {
                &mut self.mid.session_data
            }
            async fn update_session(&mut self) -> anansi::web::Result<()> {
                use anansi::records::Record;
                self.mid.session.data = self.mid.session_data.to_text()?;
                self.mid.session.raw_update(self.raw.pool()).await
            }
        }
        #[async_trait::async_trait]
        impl anansi::web::CsrfDefense for HttpRequest {
            fn token(&self) -> anansi::web::Result<anansi::web::TokenRef> {
                use anansi::util::sessions::middleware::Sessions;
                use anansi::web::TokenRef;
                Ok(TokenRef::from(self.session_data().get(TokenRef::KEY)?.as_str().unwrap()))
            }
            fn check_token(&mut self) -> anansi::web::Result<anansi::web::FormMap> {
                if let Ok(form_map) = self.raw.to_form_map() {
                    let csrf_token = form_map.get("csrf_token")?;
                    if self.token()?.check(csrf_token) {
                        *self.raw.valid_token_mut() = true;
                        return Ok(form_map);
                    }
                }
                Err(anansi::db::invalid())
            }
        }
    }
}

#[macro_export]
macro_rules! transact {
    ($req:ident, $b:expr) => {
        $req.raw().transact(async {$b}).await
    }
}

#[macro_export]
macro_rules! raw_transact {
    ($pool:ident, $b:expr) => {
        $pool.transact(async {$b}).await
    }
}

#[macro_export]
macro_rules! render {
    ($name:literal) => {
        include!(concat!("templates", anansi::main_separator!(), ".parsed", anansi::main_separator!(), $name, ".in"))
    }
}

#[macro_export]
macro_rules! app_statics {
    ($($name:ident,)*) => {
        static APP_STATICS: &[&[anansi::web::Static]] = &[
            $($name::STATICS,)*
        ];
    }
}

#[macro_export]
#[cfg(not(target_os = "windows"))]
macro_rules! main_separator {
    () => {r"/"}
}

#[macro_export]
#[cfg(target_os = "windows")]
macro_rules! main_separator {
    () => {r"\"}
}

#[macro_export]
macro_rules! statics {
    ($($name:expr,)*) => {
        pub static STATICS: &[anansi::web::Static] = &[
            $((concat!(anansi::main_separator!(), "static", anansi::main_separator!(), $name), include_bytes!(concat!("static", anansi::main_separator!(), $name))),)*
        ];
    }
}

#[macro_export]
macro_rules! err_box {
    ($e:expr) => {
        Err(Box::new($e))
    };
}

#[macro_export]
macro_rules! import {
    ($s:tt, $a:tt) => {
        anansi::web::Route::Import(($s, $a::urls::ROUTES))
    };
}

#[macro_export]
macro_rules! redirect {
    () => {
        Response::redirect("/")
    };
    ($($args:tt)*) => {
        Response::redirect(&anansi::url!($($args)*))
    };
}

#[macro_export]
macro_rules! strip_plus {
    (+ $($rest: tt)*) => {
        $($rest)*
    }
}

#[derive(Debug, Clone)]
pub struct Headers(HashMap<String, String>);

impl Headers {
    fn new() -> Self {
        Self {0: HashMap::new()}
    }
    pub fn insert(&mut self, key: String, value: String) {
        self.0.insert(key, value);
    }
    fn insert_str(&mut self, key: &str, value: &str) {
        self.0.insert(key.to_string(), value.to_string());
    }
    fn get(&self, key: &str) -> Option<&String> {
        self.0.get(key)
    }
}

impl IntoIterator for Headers {
    type Item = (String, String);
    type IntoIter = std::collections::hash_map::IntoIter<String, String>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub struct Http404 {}

impl fmt::Display for Http404 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "404 Not Found")
    }
}

impl Error for Http404 {}

#[derive(Debug)]
pub struct WebError {
    kind: WebErrorKind,
}

impl WebError {
    pub fn from(kind: WebErrorKind) -> Self {
        Self {kind}
    }
    pub fn kind(&self) -> &WebErrorKind {
        &self.kind
    }
}

impl fmt::Display for WebError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Error for WebError {}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum WebErrorKind {
    Invalid,
    NoSession,
    Unauthenticated,
}

impl fmt::Display for WebErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Invalid => "invalid data",
            Self::NoSession => "could not get session token from cookie",
            Self::Unauthenticated => "user not authenticated",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Parameters(HashMap<String, String>);

impl Parameters {
    pub fn new() -> Self {
        Self {0: HashMap::new()}
    }
    pub fn get(&self, key: &str) -> Result<&String> {
        self.0.get(key).ok_or(invalid())
    }
    pub fn remove(&mut self, key: &str) -> Result<String> {
        self.0.remove(key).ok_or(invalid())
    }
    pub fn insert(&mut self, key: String, value: String) {
        self.0.insert(key, value);
    }
    pub fn iter(&self) -> Iter<String, String> {
        self.0.iter()
    }
}

#[derive(Clone)]
pub enum Route<B: BaseRequest + 'static> {
    Path ((&'static str, View<B>)),
    Import((&'static str, &'static [Route<B>])),
}

pub fn route_path<B: BaseRequest>(s: &'static str, f: View<B>) -> Route<B> {
    Route::Path((s, f))
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Method {
    Get,
    Post,
}

#[derive(Debug, Clone)]
pub struct Body {
    body: Vec<u8>,
}

impl Body {
    pub fn as_slice(&self) -> &[u8] {
        self.body.as_slice()
    }
}

#[derive(Clone, Debug)]
pub struct Response {
    status_line: String,
    headers: Headers,
    body: Option<Body>,
}

impl Response {
    pub fn headers(&self) -> &Headers {
        &self.headers
    }
    pub fn headers_mut(&mut self) -> &mut Headers {
        &mut self.headers
    }
    pub fn new(status_line: &'static str, contents: Vec<u8>) -> Self {
        let mut headers = Headers::new();
        headers.insert_str("content-type", "text/html");
        headers.insert_str("charset", "UTF-8");
        headers.insert_str("Server", "webserver");
        headers.insert_str("content-security-policy", "default-src 'self'; script-src 'self'; connect-src 'self'; img-src 'self'; style-src 'self'; frame-ancestors 'none'; form-action 'self'; upgrade-insecure-requests;");
        headers.insert_str("x-frame-options", "DENY");
        headers.insert("Content-length".to_string(), contents.len().to_string());
        let body = Some(Body {body: contents});
        Self {status_line: status_line.to_string(), headers, body}
    }
    pub fn content(status_line: &'static str, ty: &str, contents: Vec<u8>) -> Self {
        let mut headers = Headers::new();
        headers.insert_str("Content-Type", ty);
        headers.insert("Content-Length".to_string(), contents.len().to_string());
        let body = Some(Body {body: contents});
        Self {status_line: status_line.to_string(), headers, body}
    }
    pub fn internal_error(b: &[u8]) -> Self {
        Self::new("HTTP/1.1 500 INTERNAL SERVER ERROR", b.to_vec())
    }
    pub fn redirect(location: &str) -> Self {
        let headers = Headers::new();
        Self {status_line: format!("HTTP/1.1 303 See Other\r\nLocation: {}", location), headers, body: None}
    }
    pub fn set_persistent(mut self, key: &str, value: &str, expires: &DateTime) -> Self {
        self.headers.insert("Set-Cookie".to_string(), format!("{key}={value}; Path=/; Expires={}; Secure; HttpOnly; SameSite=Lax", expires.to_gmt()));
        self
    }
    pub fn into_bytes(self) -> Vec<u8> {
        let mut s = String::from(format!("{}\r\n", self.status_line));
        for (key, value) in self.headers {
            s.push_str(&format!("{key}: {value};\r\n"));
        }
        s.push_str("\r\n");
        
        let mut bytes = s.into_bytes();
        if let Some(mut body) = self.body {
            bytes.append(&mut body.body);
        }
        bytes
    }
}

pub struct RequestLine {
    pub method: Method,
    pub url: String,
}

pub struct FormMap {
    map: HashMap<String, String>,
}

impl FormMap {
    pub fn get(&self, s: &str) -> Result<&String> {
        self.map.get(s).ok_or(invalid())
    }
    pub fn remove(&mut self, s: &str) -> Result<String> {
        self.map.remove(s).ok_or(invalid())
    }
}

#[derive(Debug, Clone)]
pub struct Cookies {
    cookies: HashMap<String, String>,
}

impl Cookies {
    pub fn get(&self, s: &str) -> Result<&String> {
        self.cookies.get(s).ok_or(invalid())
    }
    pub fn remove(&mut self, s: &str) -> Result<String> {
        self.cookies.remove(s).ok_or(invalid())
    }
}

#[derive(Debug, Clone)]
pub struct RawRequest<D: DbPool> {
    pub method: Method,
    pub url: String,
    pub headers: Headers,
    pub body: Option<Body>,
    pub cookies: Cookies,
    pub params: Parameters,
    pool: D,
    std_rng: Rng,
    valid_token: bool,
}

pub fn percent_encode(s: String) -> String {
    let mut t = String::new();
    for c in s.chars() {
        match c {
            ':' => t.push_str("%3A"),
            '/' => t.push_str("%2F"),
            '?' => t.push_str("%3F"),
            '#' => t.push_str("%23"),
            '[' => t.push_str("%5B"),
            ']' => t.push_str("%5D"),
            '@' => t.push_str("%40"),
            '!' => t.push_str("%21"),
            '$' => t.push_str("%24"),
            '&' => t.push_str("%26"),
            '\'' => t.push_str("%27"),
            '(' => t.push_str("%28"),
            ')' => t.push_str("%29"),
            '*' => t.push_str("%2A"),
            '+' => t.push_str("%2B"),
            ',' => t.push_str("%2C"),
            ';' => t.push_str("%3B"),
            '=' => t.push_str("%3D"),
            '%' => t.push_str("%25"),
            ' ' => t.push('+'),
            _ => t.push(c),
        }
    }
    t
}

pub fn percent_decode(s: &str) -> Result<String> {
    let mut t = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '%' {
            let mut s = String::new();
            s.push(chars.next().ok_or(invalid())?);
            s.push(chars.next().ok_or(invalid())?);
            match s.as_str() {
                "21" => t.push('!'), 
                "22" => t.push('"'), 
                "23" => t.push('#'), 
                "24" => t.push('$'), 
                "25" => t.push('%'), 
                "26" => t.push('&'), 
                "27" => t.push('\''), 
                "28" => t.push('('), 
                "29" => t.push(')'), 
                "2A" => t.push('*'), 
                "2B" => t.push('+'), 
                "2C" => t.push(','), 
                "2D" => t.push('-'), 
                "2E" => t.push('.'), 
                "2F" => t.push('/'), 
                "3A" => t.push(':'), 
                "3B" => t.push(';'), 
                "3C" => t.push('<'), 
                "3D" => t.push('='), 
                "3E" => t.push('>'), 
                "3F" => t.push('?'), 
                "40" => t.push('@'), 
                "5B" => t.push('['), 
                "5C" => t.push('\\'), 
                "5D" => t.push(']'), 
                "5E" => t.push('^'), 
                "5F" => t.push('_'), 
                "60" => t.push('`'), 
                "7B" => t.push('{'), 
                "7C" => t.push('|'), 
                "7D" => t.push('}'), 
                "7E" => t.push('~'), 
                _ => return Err(invalid()),
            }
        } else if c == '+' {
            t.push(' ');
        } else {
            t.push(c);
        }
    }
    Ok(t)
}

pub fn html_escape(s: &str) -> String {
    let mut escaped = String::new();
    for c in s.chars() {
        let html = match c {
            '<' => "&lt;",
            '>' => "&gt;",
            '\'' => "&#x27;",
            '"' => "&quot;",
            '&' => "&amp;",
            _ => {
                escaped.push(c);
                continue;
            }
        };
        escaped.push_str(html);
    }
    escaped
}

fn find_byte(buffer: &[u8], n: usize, byte: u8) -> usize {
    let mut m = n;
    for b in &buffer[n..] {
        if *b == byte {
            return m;
        } else {
            m += 1;
        }
    }
    buffer.len()
}

fn find_bytes(buffer: &[u8], mut n: usize, bytes: &[u8]) -> usize {
    loop {
        let m = find_byte(buffer, n, bytes[0]) + 1;
        if n >= buffer.len() - bytes.len() {
            return m;
        }
        let o = m + bytes.len() - 1;
        if buffer[m..o] == bytes[1..] {
            return o;
        } else {
            n = m + 1;
        }
    }
}

fn find_space(buffer: &[u8], n: usize) -> usize {
    find_byte(buffer, n, SPACE)
}

fn get_headers(buffer: &[u8], o: &mut usize) -> Result<Headers> {
    let mut headers = Headers::new();
    let mut n = *o;
    let mut m = find_bytes(buffer, n, b"\r\n");
    while n < buffer.len() {
        let n2 = find_bytes(buffer, n, b": ");
        let key = get_string(&buffer[n..n2-2])?;
        let value = get_string(&buffer[n2..m-2])?;
        headers.insert(key, value);
        let m2 = find_bytes(buffer, m, b"\r\n");
        if m2 - m <= 2 {
            break;
        } else {
            n = m;
            m = m2;
        }
    }
    *o = m;
    Ok(headers)
}

pub fn get_string(buffer: &[u8]) -> result::Result<String, FromUtf8Error> {
    String::from_utf8(buffer.to_vec())
}

pub trait Reverse {
    fn reverse(&self, view: View<Self>, args: &[&dyn fmt::Display]) -> String;
}

#[async_trait]
pub trait GetRecord: BaseRequest {
    async fn get_record<F: FromParams + Send + Sync>(&self) -> Result<F> where Self: Sized {
        F::from_params(self.params()).await?.get(self).await
    }
}

#[async_trait]
pub trait BaseRequest: Send + Sync {
    type Mid: BaseMiddleware;
    type Usr: BaseUser;
    type SqlPool: DbPool;
    type Cache: BaseCache;

    async fn new(buffer: &[u8], request_line: RequestLine, url: Arc<HashMap<usize, Vec<String>>>, pool: Self::SqlPool, cache: Self::Cache, rng: Rng, admin: AdminRef<Self>) -> Result<Self> where Self: Sized;
    fn method(&self) -> &Method;
    fn url(&self) -> &String;
    fn headers(&self) -> &Headers;
    fn body(&self) -> &Option<Body>;
    fn cookies(&self) -> &Cookies;
    fn params(&self) -> &Parameters;
    fn params_mut(&mut self) -> &mut Parameters;
    fn raw(&self) -> &RawRequest<Self::SqlPool>;
    fn mid(&self) -> &Self::Mid;
    fn to_form_map(&self) -> Result<FormMap>;
    fn from(raw: RawRequest<Self::SqlPool>, mid: Self::Mid, cache: Self::Cache, urls: Arc<HashMap<usize, Vec<String>>>, admin: AdminRef<Self>) -> Self where Self: Sized;
    fn user(&self) -> &Self::Usr;
    fn user_mut(&mut self) -> &mut Self::Usr;
    fn cache(&self) -> &Self::Cache;
    fn cache_mut(&mut self) -> &mut Self::Cache;
    fn to_raw(self) -> RawRequest<Self::SqlPool>;
    async fn handle_no_session(response: Response, pool: Self::SqlPool, std_rng: Rng) -> Result<Response> where Self: Sized;
}

fn parse_query_string(qs: &str) -> Option<Vec<(String, String)>> {
    let qv: Vec<&str> = qs.split('&').collect();
    let mut queries = vec![];
    for q in qv {
        if let Some((key, value)) = q.split_once('=') {
            if let Ok(k) = percent_decode(key) {
                if let Ok(val) = percent_decode(value) {
                    queries.push((k, val));
                } else {
                    return None;
                }
            } else {
                return None;
            }
        } else {
            return None;
        }
    }
    Some(queries)
}

pub async fn route_request<B: BaseRequest + fmt::Debug + 'static>(dirs: Vec<&str>, req: &mut B, routes: &Routes<B>) -> Result<Response> {
    for (patterns, view) in routes {
        if patterns.len() == dirs.len() {
            let mut b = true;
            let mut params = Parameters::new();
            for (pattern, dir) in patterns.iter().zip(dirs.iter()) {
                match pattern.as_bytes()[0] {
                    SLASH => if pattern == dir {
                        continue;
                    } else if let Some((d, qs)) = dir.split_once('?') {
                        if pattern == d {
                            if let Some(qv) = parse_query_string(qs) {
                                for (k, v) in qv {
                                    params.insert(k, v);
                                }
                                break;
                            }
                        }
                        b = false;
                        break;
                    } else {
                        b = false;
                        break;
                    }
                    LEFT_BRACE => {
                        params.insert(pattern[1..].to_string(), dir[1..].to_string());
                    }
                    _ => return Err(invalid()),
                }
            }
            if b {
                *req.params_mut() = params;
                return view(req).await;
            }
        }
    }
    Err(Box::new(anansi::web::Http404 {}))
}

impl<M, U, S, C> fmt::Debug for dyn BaseRequest<Mid = M, Usr = U, SqlPool = S, Cache = C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BaseRequest")
    }
}

#[macro_export]
macro_rules! path {
    ($s:literal, $f:expr) => {
        anansi::web::Route::Path(($s, $f))
    }
}

#[macro_export]
macro_rules! request_derive {
    () => {
        impl anansi::web::Reverse for HttpRequest {
            fn reverse(&self, view: anansi::web::View<Self>, disps: &[&dyn std::fmt::Display]) -> String {
                let mut disp = disps.iter();
                let patterns = self.urls.get(&(view as anansi::web::View<Self> as usize)).expect("could not get url");
                let mut s = String::new();
                for pattern in patterns {
                    match pattern.as_bytes()[0] {
                        anansi::web::SLASH => {
                            s.push_str(pattern);
                        }
                        anansi::web::LEFT_BRACE => {
                            let d = format!("/{}", disp.next().expect("too few arguments to reverse url").to_string());
                            s.push_str(&d);
                        }
                        _ => panic!("could not reverse url"),
                    }
                }
                s
            }
        }
        #[async_trait::async_trait]
        impl anansi::web::GetRecord for HttpRequest {}
        #[async_trait::async_trait]
        impl anansi::web::BaseRequest for HttpRequest {
            type Mid = Middleware;
            type Usr = anansi::util::auth::records::User;
            type SqlPool = Pool;
            type Cache = AppCache;

            async fn new(buffer: &[u8], request_line: $crate::web::RequestLine, urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>, pool: Self::SqlPool, cache: Self::Cache, std_rng: $crate::server::Rng, admin: $crate::admin_site::AdminRef<Self>) -> $crate::web::Result<Self> where Self: Sized {
                let mut raw = $crate::web::RawRequest::new(buffer, request_line, pool, std_rng).await?;
                let mid = Middleware::new(&mut raw).await?;
                Ok(Self {raw, mid, urls, cache, admin})
            }
            fn method(&self) -> &anansi::web::Method {
                &self.raw.method
            }
            fn url(&self) -> &String {
                &self.raw.url
            }
            fn headers(&self) -> &anansi::web::Headers {
                &self.raw.headers
            }
            fn body(&self) -> &Option<anansi::web::Body> {
                &self.raw.body
            }
            fn cookies(&self) -> &anansi::web::Cookies {
                &self.raw.cookies
            }
            fn params(&self) -> &anansi::web::Parameters {
                &self.raw.params
            }
            fn params_mut(&mut self) -> &mut anansi::web::Parameters {
                &mut self.raw.params
            }
            fn mid(&self) -> &Self::Mid {
                &self.mid
            }
            fn raw(&self) -> &anansi::web::RawRequest<Pool> {
                &self.raw
            }
            fn to_form_map(&self) -> anansi::web::Result<anansi::web::FormMap> {
                self.raw().to_form_map()
            }
            fn from(raw: anansi::web::RawRequest<Pool>, mid: Middleware, cache: Self::Cache, urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>, admin: $crate::admin_site::AdminRef<Self>) -> Self {
                Self{raw, mid, cache, urls, admin}
            }
            fn to_raw(self) -> anansi::web::RawRequest<Pool> {
                self.raw
            }
            fn user(&self) -> &Self::Usr {
                &self.mid.user
            }
            fn user_mut(&mut self) -> &mut Self::Usr {
                &mut self.mid.user
            }
            fn cache(&self) -> &Self::Cache {
                &self.cache
            }
            fn cache_mut(&mut self) -> &mut Self::Cache {
                &mut self.cache
            }
            async fn handle_no_session(response: anansi::web::Response, pool: Self::SqlPool, std_rng: anansi::server::Rng) -> anansi::web::Result<anansi::web::Response> {
                let session = anansi::util::sessions::records::Session::gen(&pool, &std_rng).await?;
                Ok(response.set_persistent("st", &session.secret, &session.expires))
            }
        }
    }
}

impl<D: DbPool> RawRequest<D> {
    pub fn get_request_line(buffer: &[u8]) -> Result<(usize, RequestLine)> {
        let mut m = find_space(buffer, 0);
        let method = match &buffer[..m] {
            b"GET" => Method::Get,
            b"POST" => Method::Post,
            _ => return Err(invalid())
        };
        let mut n = m + 1;
        m = find_space(buffer, n);
        let url = get_string(&buffer[n..m])?;
        n = m + 1;
        m = find_bytes(buffer, n, b"\r\n");
        let version = get_string(&buffer[n..m-2])?;
        if version != "HTTP/1.1" {
            return Err(invalid());
        }
        Ok((m, RequestLine{method, url}))
    }
    fn get_cookies(headers: &Headers) -> Result<Cookies> {
        let mut cookies = HashMap::new();
        if let Some(buffer) = headers.get("Cookie") {
            let buffer = buffer.as_bytes();
            let mut n = 0;
            while n < buffer.len() {
                let mut m = find_byte(buffer, n, EQUAL);
                let key = get_string(&buffer[n..m])?;
                n = m + 1;
                m = find_byte(buffer, n, SEMICOLON);
                let value = get_string(&buffer[n..m])?;
                cookies.insert(key, value);
                n = m + 2;
            }
        }
        Ok(Cookies {cookies})
    }
    pub fn to_form_map(&self) -> Result<FormMap> {
        if self.method == Method::Post {
            if let Some(body) = &self.body {
                let buffer = body.as_slice();
                let mut n = 0;
                let mut map = HashMap::new();
                while n < buffer.len() {
                    let mut m = find_byte(buffer, n, EQUAL);
                    let key = get_string(&buffer[n..m])?;
                    n = m + 1;
                    m = find_byte(buffer, n, AMPERSAND);
                    let value = get_string(&buffer[n..m])?;
                    let decoded = percent_decode(&value)?;
                    map.insert(key, decoded);
                    n = m + 1;
                }
                return Ok(FormMap {map})
            }
        }
        Err(invalid())
    }
    pub async fn new(buffer: &[u8], request_line: RequestLine, pool: D, std_rng: Rng) -> Result<Self> {
        let mut n = 0;
        let headers = get_headers(buffer, &mut n)?;
        n += 2;
        let body = if n == buffer.len() {
            None
        } else {
            Some(Body {body: buffer[n..].to_vec()})
        };
        let cookies = Self::get_cookies(&headers)?;
        let params = Parameters::new();
        Ok(Self {
            method: request_line.method,
            url: request_line.url,
            headers,
            body,
            cookies,
            params,
            pool,
            std_rng,
            valid_token: false,
        })
    }
    pub fn cookies_mut(&mut self) -> &mut Cookies {
        &mut self.cookies
    }
    pub fn pool(&self) -> &D {
        &self.pool
    }
    pub fn rng(&self) -> &Rng {
        &self.std_rng
    }
    pub fn valid_token(&self) -> bool {
        self.valid_token
    }
    pub fn valid_token_mut(&mut self) -> &mut bool {
        &mut self.valid_token
    }
    pub fn url(&self) -> &String {
        &self.url
    }
    pub async fn transact<F: Future<Output = Result<O>> + Send, O: Send>(&self, future: F) -> F::Output {
        self.pool().transact(future).await
    }
}

pub trait BaseUser: Record<Pk = BigInt> + Send + Sync {
    type Name: fmt::Display;
    fn username(&self) -> &Self::Name;
    fn is_auth(&self) -> bool;
}

pub trait BaseMiddleware: Sync + Send {}

pub struct TokenRef<'a> {
    secret: &'a str,
}

impl<'a> TokenRef<'a> {
    pub const KEY: &'static str = "_csrf_token";
    pub fn from(secret: &'a str) -> Self {
        Self {secret}
    }
    pub fn check(&self, s: &str) -> bool {
        s == self.secret
    }
}

impl<'a> fmt::Display for TokenRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.secret)
    }
}

#[async_trait::async_trait]
pub trait CsrfDefense {
    fn token(&self) -> Result<TokenRef>;
    fn check_token(&mut self) -> Result<FormMap>;
}
