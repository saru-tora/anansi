use async_trait::async_trait;
use std::sync::Arc;
use std::collections::{HashMap, hash_map::Iter};
use crate::db::AsDb;
use std::string::FromUtf8Error;
use std::error::Error;
use std::result;
use std::fmt;
use std::pin::Pin;
use std::future::Future;
use std::path::PathBuf;

use hyper::{Request as RawHyperRequest, Response as RawHyperResponse, body::Incoming as IncomingBody};
use hyper::{header::HeaderValue, body::Bytes, StatusCode};
use http_body_util::{BodyExt, Full};

use crate::cache::BaseCache;
use crate::server::{Rng, Settings};
use crate::router::Routes;
use crate::records::{Record, FromParams, BigInt, DateTime};
use crate::email::{EmailBuilder, Mailer};

#[cfg(not(feature = "minimal"))]
use crate::admin_site::AdminRef;

pub type Result<T> = result::Result<T, Box<dyn Error + Send + Sync>>;
pub type View<B> = fn(&mut B) -> Pin<Box<dyn Future<Output = Result<Response>> + Send + '_>>;
pub type Static = (&'static str, &'static [u8]);

const AMPERSAND: u8 = 38;
const SEMICOLON: u8 = 59;
const EQUAL: u8 = 61;

pub const SLASH: u8 = 47;
pub const LEFT_BRACE: u8 = 123;

pub const GET: Method = Method::GET;
pub const POST: Method = Method::POST;

pub use hyper::{Uri, Method, HeaderMap};

#[derive(Debug)]
pub struct HyperRequest(pub(crate) RawHyperRequest<IncomingBody>);

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
    pub use anansi::web::BaseRequest;
    pub use anansi::router::Router;
}

#[async_trait]
pub trait Middleware {
    async fn new<D: AsDb>(raw: &mut RawRequest<D>) -> Self;
}

#[async_trait]
pub trait Service<R: BaseRequest>: Send + Sync {
    type S: Service<R>;
    async fn init(service: Self::S, settings: &Settings) -> Self;
    async fn call(&self, view: &View<R>, req: &mut R) -> Result<Response>;
}

pub struct ViewService;

#[async_trait]
impl<R: BaseRequest> Service<R> for ViewService {
    type S = Self;
    async fn init(_service: Self, _settings: &Settings) -> Self {
        Self {}
    }
    async fn call(&self, view: &View<R>, req: &mut R) -> Result<Response> {
        view(req).await
    }
}

pub struct SecurityHeaders<S> {
    service: S,
}

#[async_trait]
impl<Svc: Service<R>, R: BaseRequest> Service<R> for SecurityHeaders<Svc> {
    type S = Svc;
    async fn init(service: Svc, _settings: &Settings) -> Self {
        Self {service}
    }
    async fn call(&self, view: &View<R>, req: &mut R) -> Result<Response> {
        let mut resp = self.service.call(view, req).await?;
        {
            let headers = resp.headers_mut();
            headers.insert("content-security-policy", "default-src 'self'; script-src 'self' 'wasm-unsafe-eval'; connect-src 'self'; img-src 'self' data:; style-src 'self'; frame-ancestors 'none'; form-action 'self'; upgrade-insecure-requests;".parse()?);
            headers.insert("x-frame-options", "DENY".parse()?);
        }
        Ok(resp)
    }
}

#[cfg(not(feature = "minimal"))]
#[macro_export]
macro_rules! setup {
    ($($name:ident, $ty:path, $tr:path)*) => {
        #[derive(Debug, Clone)]
        pub struct AppData {
            pool: Pool,
        }
        impl AppData {
            pub async fn new() -> Self {
                let pool = anansi::server::get_db::<AppData>(crate::app_migrations).await;
                Self {pool}
            }
        }
        impl anansi::db::AsDb for AppData {
            type SqlDb = Pool;
            fn as_db(&self) -> &Pool {
                &self.pool
            }
            fn as_db_mut(&mut self) -> &mut Pool {
                &mut self.pool
            }
        }
        #[derive(Debug, Clone)]
        pub struct HttpRequest {
            raw: anansi::web::RawRequest<AppData>,
            mid: AppMiddleware,
            urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>,
            cache: AppCache,
            admin: anansi::util::admin::site::AdminRef<Self>,
            mailer: Option<anansi::email::Mailer>,
            $($name: $ty,)*
        }
        #[derive(Debug, Clone)]
        pub struct AppMiddleware {
            user: anansi::util::auth::records::User,
            session: anansi::util::sessions::records::Session,
            session_data: anansi::util::sessions::records::SessionData,
        }
        #[cfg(not(test))]
        async fn get_session(raw: &mut anansi::web::RawRequest<AppData>) -> $crate::web::Result<anansi::util::sessions::records::Session> {
            match anansi::util::sessions::records::Session::from_raw(raw).await {
                Ok(s) => Ok(s),
                Err(_) => return Err(Box::new(anansi::web::WebError::from(anansi::web::WebErrorKind::NoSession))),
            }
        }
        #[cfg(test)]
        async fn get_session(raw: &mut anansi::web::RawRequest<AppData>) -> $crate::web::Result<anansi::util::sessions::records::Session> {
            Ok(anansi::util::sessions::records::Session::test())
        }
        impl AppMiddleware {
            pub async fn new(raw: &mut anansi::web::RawRequest<AppData>) -> $crate::web::Result<Self> {
                use anansi::records::{Record, DataType};
                let session = get_session(raw).await?;
                let session_data = session.to_data()?;
                let user_id = session_data.get(anansi::util::auth::records::User::KEY)?.as_i64().expect("could not get user id");
                let user = if user_id == 0 {
                    anansi::util::auth::records::User::guest()
                } else {
                    anansi::util::auth::records::User::find(anansi::records::BigInt::from_val(user_id)?).raw_get(raw.pool()).await?
                };
                Ok(AppMiddleware {user, session, session_data})
            }
        }
        impl anansi::web::BaseMiddleware for AppMiddleware {}
        anansi::request_derive!($($name, $ty,)*);
        pub trait Request: anansi::web::BaseRequest + anansi::util::sessions::middleware::Sessions + anansi::util::auth::middleware::Auth + anansi::util::admin::site::HasAdmin + anansi::web::CsrfDefense  + anansi::web::Reverse + std::fmt::Debug + anansi::web::GetRecord $(+ $tr)* {}
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
                anansi::err_box!(anansi::web::WebError::from(anansi::web::WebErrorKind::BadToken))
            }
        }
    }
}

#[cfg(feature = "minimal")]
#[macro_export]
macro_rules! setup {
    ($($name:ident, $ty:path, $tr:path)*) => {
        #[derive(Debug, Clone)]
        pub struct HttpRequest {
            raw: anansi::web::RawRequest<AppData>,
            mid: AppMiddleware,
            urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>,
            cache: AppCache,
            mailer: Option<anansi::email::Mailer>,
            $($name: Option<$ty>,)*
        }
        #[derive(Debug, Clone)]
        pub struct AppMiddleware {}
        impl AppMiddleware {
            pub async fn new(_raw: &mut anansi::web::RawRequest<AppData>) -> $crate::web::Result<Self> {
                Ok(AppMiddleware {})
            }
        }
        impl anansi::web::BaseMiddleware for AppMiddleware {}
        anansi::request_derive!($($name, $ty)*);
        pub trait Request: anansi::web::BaseRequest<SqlPool = AppData> + anansi::web::Reverse + std::fmt::Debug + anansi::web::GetRecord $(+ $tr)* {}
        impl Request for HttpRequest {}
    }
}

#[macro_export]
macro_rules! transact {
    ($req:ident, $b:expr) => {
        {
            use anansi::db::DbPool;
            match $req.raw().pool().begin().await {
                Ok(n) => match async {$b}.await {
                    Ok(r) => match $req.raw().pool().commit(n).await {
                        Ok(_) => Ok(r),
                        Err(e) => Err(e),
                    }
                    Err(e) => Err(e),
                }
                Err(e) => Err(e),
            }
        }
    }
}

#[macro_export]
macro_rules! raw_transact {
    ($pool:ident, $b:expr) => {
        {
            match $pool.raw_execute("BEGIN;").await {
                Ok(r) => match async {$b}.await {
                    Ok(_) => match $pool.raw_execute("COMMIT;").await {
                        Ok(_) => Ok(r),
                        Err(e) => Err(e),
                    }
                    Err(e) => Err(e),
                }
                Err(e) => Err(e),
            }
        }
    }
}

#[macro_export]
macro_rules! raw_render {
    ($name:literal) => {
        include!(concat!("templates", anansi::main_separator!(), ".parsed", anansi::main_separator!(), $name, ".in"))
    }
}

#[macro_export]
macro_rules! render {
    ($name:literal) => {
        Ok(anansi::web::Response::new(200, anansi::raw_render!($name)))
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
pub struct Headers(HeaderMap<HeaderValue>);

impl Headers {
    pub fn insert(&mut self, key: &'static str, value: &str) {
        self.0.insert(key, HeaderValue::from_str(value).unwrap());
    }
}

impl IntoIterator for Headers {
    type Item = (Option<hyper::header::HeaderName>, HeaderValue);
    type IntoIter = hyper::header::IntoIter<HeaderValue>;
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
    BadToken,
    NoParam,
    NoField,
    NoCookie,
    NoAttribute,
    BadDecode,
    BadUri,
    BadForm,
    BadName,
    BadPath,
    NoExtension,
    BadExtension,
    BadCapture,
    BadSplit,
    BadEmail,
    BadMailer,
    BadFill,
    BadValidate,
    NoCache,
    FieldError,
    BadField,
    NoPermission,
    BadDb,
    NoData,
    ExpiredSession,
    NotAdmin,
    BadRelation,
    BadPassword,
    BadTotp,
    BadUsername,
    BadDateTime,
    BadBegin,
    BadCommit,
    NoQr,
}

impl fmt::Display for WebErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Invalid => "invalid data",
            Self::NoSession => "could not get session token from cookie",
            Self::Unauthenticated => "user is not authenticated",
            Self::BadToken => "bad CSRF token",
            Self::NoParam => "parameter does not exist",
            Self::NoField => "form field does not exist",
            Self::NoCookie => "cookie does not exist",
            Self::NoAttribute => "attribute does not exist",
            Self::BadDecode => "could not percent decode string",
            Self::BadUri => "unrecognized uri",
            Self::BadForm => "could not create form map",
            Self::BadName => "file name has bad character(s)",
            Self::BadPath => "file path is not valid",
            Self::NoExtension => "file does not have extension",
            Self::BadExtension => "file does not have a valid extension",
            Self::BadCapture => "could not capture parameter",
            Self::BadSplit => "could not split url",
            Self::NoCache => "could not get cache entry",
            Self::BadEmail => "could not send email",
            Self::BadMailer => "could not create mailer",
            Self::BadFill => "could not fill form",
            Self::BadValidate => "could not validate form",
            Self::FieldError => "form has field error(s)",
            Self::BadField => "form has problem with field",
            Self::NoPermission => "user does not have permission to access resource",
            Self::BadDb => "could not connect to database",
            Self::NoData => "could not find data in session",
            Self::ExpiredSession => "session has expired",
            Self::NotAdmin => "user is not an admin",
            Self::NoQr => "could not get qr code",
            Self::BadRelation => "could not check relation",
            Self::BadPassword => "could not verify password",
            Self::BadTotp => "could not verify totp",
            Self::BadUsername => "problem with username",
            Self::BadDateTime => "could not create datetime",
            Self::BadBegin => "could not begin transaction",
            Self::BadCommit => "could not commit transaction",
        };
        write!(f, "{}", s)
    }
}

impl WebErrorKind {
    pub fn to_box(self) -> Box<WebError> {
        Box::new(WebError::from(self))
    }
}

#[derive(Debug, Clone)]
pub struct Parameters(HashMap<String, String>);

impl Parameters {
    pub fn new() -> Self {
        Self {0: HashMap::new()}
    }
    pub fn get(&self, key: &str) -> Result<&String> {
        if let Some(s) = self.0.get(key) {
            Ok(s)
        } else {
            Err(WebErrorKind::NoParam.to_box())
        }
    }
    pub fn remove(&mut self, key: &str) -> Result<String> {
        if let Some(s) = self.0.remove(key) {
            Ok(s)
        } else {
            Err(WebErrorKind::NoParam.to_box())
        }
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
    Path((&'static str, View<B>)),
    Import((&'static str, Vec<Route<B>>)),
}

pub fn route_path<B: BaseRequest>(s: &'static str, f: View<B>) -> Route<B> {
    Route::Path((s, f))
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

#[derive(Debug)]
pub struct Response(RawHyperResponse<Full<Bytes>>);

impl Response {
    pub fn status(&self) -> StatusCode {
        self.0.status()
    }
    pub fn headers(&self) -> &HeaderMap {
        self.0.headers()
    }
    pub fn headers_mut(&mut self) -> &mut HeaderMap {
        self.0.headers_mut()
    }
    pub fn new(status: u16, contents: Vec<u8>) -> Self {
        let builder = RawHyperResponse::builder()
            .status(status)
            .header("content-type", "text/html; charset=utf-8")
            .header("Server", "webserver")
            .header("Content-length", contents.len().to_string())
            .body(Full::new(Bytes::from(contents))).unwrap();
        Self {0: builder}
    }
    pub fn json<T: serde::ser::Serialize>(value: &T) -> Result<Self> {
        let contents = serde_json::to_vec(value)?;
        Self::json_bytes(contents)
    }
    pub fn json_bytes(contents: Vec<u8>) -> Result<Self> {
        let builder = RawHyperResponse::builder()
            .header("content-type", "application/json")
            .header("Server", "webserver")
            .header("Content-length", contents.len().to_string())
            .body(Full::new(Bytes::from(contents))).unwrap();
        Ok(Self {0: builder})
    }
    pub fn text(contents: String) -> Self {
        let contents = contents.into_bytes();
        let builder = RawHyperResponse::builder()
            .header("content-type", "text/plain")
            .header("Server", "webserver")
            .header("Content-length", contents.len().to_string())
            .body(Full::new(Bytes::from(contents))).unwrap();
        Self {0: builder}
    }
    pub fn content(status: u16, ty: &str, contents: Vec<u8>) -> Self {
        let builder = RawHyperResponse::builder()
            .status(status)
            .header("Content-Type", ty)
            .header("Content-Length", contents.len().to_string())
            .body(Full::new(Bytes::from(contents))).unwrap();
        Self {0: builder}
    }
    pub fn internal_error(b: Vec<u8>) -> Self {
        let builder = RawHyperResponse::builder()
            .status(500)
            .body(Full::new(Bytes::from(b))).unwrap();
        Self {0: builder}
    }
    pub fn redirect(location: &str) -> Self {
        let builder = RawHyperResponse::builder()
            .status(303)
            .header("Location", location)
            .body(Full::new(Bytes::new()))
            .unwrap();
        Self {0: builder}
    }
    pub fn set_persistent(mut self, key: &str, value: &str, expires: &DateTime) -> Self {
        self.0.headers_mut().insert("Set-Cookie", HeaderValue::from_str(&format!("{key}={value}; Path=/; Expires={}; Secure; HttpOnly; SameSite=Lax", expires.to_gmt())).unwrap());
        self
    }
    pub(crate) fn into_inner(self) -> RawHyperResponse<Full<Bytes>> {
        self.0
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
        if let Some(v) = self.map.get(s) {
            Ok(v)
        } else {
            Err(WebErrorKind::NoField.to_box())
        }
    }
    pub fn remove(&mut self, s: &str) -> Result<String> {
        if let Some(v) = self.map.remove(s) {
            Ok(v)
        } else {
            Err(WebErrorKind::NoField.to_box())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cookies {
    cookies: HashMap<String, String>,
}

impl Cookies {
    pub fn get(&self, s: &str) -> Result<&String> {
        if let Some(v) = self.cookies.get(s) {
            Ok(v)
        } else {
            Err(WebErrorKind::NoCookie.to_box())
        }
    }
    pub fn remove(&mut self, s: &str) -> Result<String> {
        if let Some(v) = self.cookies.remove(s) {
            Ok(v)
        } else {
            Err(WebErrorKind::NoCookie.to_box())
        }
    }
}

#[derive(Debug, Clone)]
pub struct RawRequest<D: AsDb> {
    pub method: Method,
    pub url: Uri,
    pub headers: HeaderMap<HeaderValue>,
    pub body: Option<Body>,
    pub cookies: Cookies,
    pub params: Parameters,
    app_state: Arc<D>,
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
            let c = if let Some(c) = chars.next() {
                c
            } else {
                return Err(WebErrorKind::BadDecode.to_box());
            };
            s.push(c);
            let d = if let Some(d) = chars.next() {
                d
            } else {
                return Err(WebErrorKind::BadDecode.to_box());
            };
            s.push(d);
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
                _ => return Err(WebErrorKind::BadDecode.to_box()),
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

pub fn html_escape2(view: &mut String, s: &str) {
    for c in s.chars() {
        let html = match c {
            '<' => "&lt;",
            '>' => "&gt;",
            '\'' => "&#x27;",
            '"' => "&quot;",
            '&' => "&amp;",
            _ => {
                view.push(c);
                continue;
            }
        };
        view.push_str(html);
    }
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

#[cfg(not(feature = "minimal"))]
#[async_trait]
pub trait BaseRequest: Send + Sync {
    type Mid: BaseMiddleware;
    type Usr: BaseUser;
    type SqlPool: AsDb;
    type Cache: BaseCache;

    async fn new(request: HyperRequest, url: Arc<HashMap<usize, Vec<String>>>, pool: Arc<Self::SqlPool>, cache: Self::Cache, rng: Rng, admin: AdminRef<Self>, mailer: Option<Mailer>) -> Result<Self> where Self: Sized;
    fn method(&self) -> &Method;
    fn url(&self) -> &str;
    fn query(&self) -> Option<&str>;
    fn headers(&self) -> &HeaderMap;
    fn body(&self) -> &Option<Body>;
    fn cookies(&self) -> &Cookies;
    fn params(&self) -> &Parameters;
    fn params_mut(&mut self) -> &mut Parameters;
    fn raw(&self) -> &RawRequest<Self::SqlPool>;
    fn raw_mut(&mut self) -> &mut RawRequest<Self::SqlPool>;
    fn mid(&self) -> &Self::Mid;
    fn to_form_map(&self) -> Result<FormMap>;
    fn from(raw: RawRequest<Self::SqlPool>, mid: Self::Mid, cache: Self::Cache, urls: Arc<HashMap<usize, Vec<String>>>, admin: AdminRef<Self>, mailer: Option<Mailer>) -> Self where Self: Sized;
    fn user(&self) -> &Self::Usr;
    fn user_mut(&mut self) -> &mut Self::Usr;
    fn cache(&self) -> &Self::Cache;
    fn cache_mut(&mut self) -> &mut Self::Cache;
    fn email(&self) -> EmailBuilder;
    fn to_raw(self) -> RawRequest<Self::SqlPool>;
    async fn handle_no_session(response: Response, pool: Arc<Self::SqlPool>, std_rng: Rng) -> Result<Response> where Self: Sized;
}

#[cfg(feature = "minimal")]
#[async_trait]
pub trait BaseRequest: Send + Sync {
    type Mid: BaseMiddleware;
    type Usr: BaseUser;
    type SqlPool: AsDb;
    type Cache: BaseCache;

    async fn new(request: HyperRequest, url: Arc<HashMap<usize, Vec<String>>>, pool: std::sync::Arc<Self::SqlPool>, cache: Self::Cache, rng: Rng, mailer: Option<Mailer>) -> Result<Self> where Self: Sized;
    fn method(&self) -> &Method;
    fn url(&self) -> &str;
    fn query(&self) -> Option<&str>;
    fn headers(&self) -> &HeaderMap;
    fn body(&self) -> &Option<Body>;
    fn cookies(&self) -> &Cookies;
    fn params(&self) -> &Parameters;
    fn params_mut(&mut self) -> &mut Parameters;
    fn raw(&self) -> &RawRequest<Self::SqlPool>;
    fn raw_mut(&mut self) -> &mut RawRequest<Self::SqlPool>;
    fn mid(&self) -> &Self::Mid;
    fn to_form_map(&self) -> Result<FormMap>;
    fn from(raw: RawRequest<Self::SqlPool>, mid: Self::Mid, cache: Self::Cache, urls: Arc<HashMap<usize, Vec<String>>>, mailer: Option<Mailer>) -> Self where Self: Sized;
    fn user(&self) -> &Self::Usr;
    fn cache(&self) -> &Self::Cache;
    fn cache_mut(&mut self) -> &mut Self::Cache;
    fn email(&self) -> EmailBuilder;
    fn to_raw(self) -> RawRequest<Self::SqlPool>;
    async fn handle_no_session(response: Response, pool: std::sync::Arc<Self::SqlPool>, std_rng: Rng) -> Result<Response> where Self: Sized;
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

pub async fn route_request<B: BaseRequest + fmt::Debug + 'static, S: Service<B>>(dirs: Vec<&str>, req: &mut B, routes: &Routes<B>, service: &S) -> Result<Response> {
    for (patterns, view) in routes {
        if patterns.len() == dirs.len() {
            let mut b = true;
            let mut params = Parameters::new();
            for (pattern, dir) in patterns.iter().zip(dirs.iter()) {
                match pattern.as_bytes()[0] {
                    SLASH => if pattern == dir {
                        if let Some(qs) = req.query() {
                            if pattern == dir {
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
                            continue;
                        }
                    } else {
                        b = false;
                        break;
                    }
                    LEFT_BRACE => {
                        params.insert(pattern[1..].to_string(), dir[1..].to_string());
                    }
                    _ => return Err(WebErrorKind::BadUri.to_box()),
                }
            }
            if b {
                *req.params_mut() = params;
                return service.call(view, req).await;
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

#[cfg(not(feature = "minimal"))]
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
            type Mid = AppMiddleware;
            type Usr = anansi::util::auth::records::User;
            type SqlPool = AppData;
            type Cache = AppCache;

            async fn new(request: $crate::web::HyperRequest, urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>, pool: std::sync::Arc<Self::SqlPool>, cache: Self::Cache, std_rng: $crate::server::Rng, admin: $crate::admin_site::AdminRef<Self>, mailer: Option<anansi::email::Mailer>) -> $crate::web::Result<Self> where Self: Sized {
                let mut raw = $crate::web::RawRequest::new(request, pool, std_rng).await?;
                let mid = AppMiddleware::new(&mut raw).await?;
                Ok(Self {raw, mid, urls, cache, admin, mailer})
            }
            fn method(&self) -> &anansi::web::Method {
                self.raw.method()
            }
            fn url(&self) -> &str {
                self.raw.url()
            }
            fn query(&self) -> Option<&str> {
                self.raw.query()
            }
            fn headers(&self) -> &anansi::web::HeaderMap {
                self.raw.headers()
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
            fn raw(&self) -> &anansi::web::RawRequest<AppData> {
                &self.raw
            }
            fn raw_mut(&mut self) -> &mut anansi::web::RawRequest<AppData> {
                &mut self.raw
            }
            fn to_form_map(&self) -> anansi::web::Result<anansi::web::FormMap> {
                self.raw().to_form_map()
            }
            fn from(raw: anansi::web::RawRequest<AppData>, mid: AppMiddleware, cache: Self::Cache, urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>, admin: $crate::admin_site::AdminRef<Self>, mailer: Option<anansi::email::Mailer>) -> Self {
                Self{raw, mid, cache, urls, admin, mailer}
            }
            fn to_raw(self) -> anansi::web::RawRequest<AppData> {
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
            fn email(&self) -> anansi::email::EmailBuilder {
                anansi::email::Email::builder(&self.mailer)
            }
            async fn handle_no_session(response: anansi::web::Response, pool: std::sync::Arc<Self::SqlPool>, std_rng: anansi::server::Rng) -> anansi::web::Result<anansi::web::Response> {
                use anansi::db::AsDb;
                let session = anansi::util::sessions::records::Session::gen(pool.as_db(), &std_rng).await?;
                Ok(response.set_persistent("st", &session.secret, &session.expires))
            }
        }
    }
}

#[cfg(feature = "minimal")]
#[macro_export]
macro_rules! request_derive {
    ($($name:ident, $ty:path)*) => {
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
            type Mid = AppMiddleware;
            type Usr = anansi::dummy::records::DummyUser;
            type SqlPool = AppData;
            type Cache = AppCache;

            async fn new(request: $crate::web::HyperRequest, urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>, pool: std::sync::Arc<Self::SqlPool>, cache: Self::Cache, std_rng: $crate::server::Rng, mailer: Option<anansi::email::Mailer>) -> $crate::web::Result<Self> where Self: Sized {
                let mut raw = $crate::web::RawRequest::new(request, pool, std_rng).await?;
                let mid = AppMiddleware::new(&mut raw).await?;
                $(let $name = Some(<$ty>::new(&mut raw).await?);)*
                Ok(Self {raw, mid, urls, cache, mailer $(, $name)*})
            }
            fn method(&self) -> &anansi::web::Method {
                self.raw.method()
            }
            fn url(&self) -> &str {
                self.raw.url()
            }
            fn query(&self) -> Option<&str> {
                self.raw.query()
            }
            fn headers(&self) -> &anansi::web::HeaderMap {
                self.raw.headers()
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
            fn raw(&self) -> &anansi::web::RawRequest<AppData> {
                &self.raw
            }
            fn raw_mut(&mut self) -> &mut anansi::web::RawRequest<AppData> {
                &mut self.raw
            }
            fn to_form_map(&self) -> anansi::web::Result<anansi::web::FormMap> {
                self.raw().to_form_map()
            }
            fn from(raw: anansi::web::RawRequest<AppData>, mid: AppMiddleware, cache: Self::Cache, urls: std::sync::Arc<std::collections::HashMap<usize, Vec<String>>>, mailer: Option<anansi::email::Mailer>) -> Self {
                Self{raw, mid, cache, urls, mailer $(, $name: None)*}
            }
            fn to_raw(self) -> anansi::web::RawRequest<AppData> {
                self.raw
            }
            fn user(&self) -> &Self::Usr {
                unimplemented!()
            }
            fn cache(&self) -> &Self::Cache {
                &self.cache
            }
            fn cache_mut(&mut self) -> &mut Self::Cache {
                &mut self.cache
            }
            fn email(&self) -> anansi::email::EmailBuilder {
                anansi::email::Email::builder(&self.mailer)
            }
            async fn handle_no_session(response: anansi::web::Response, pool: std::sync::Arc<Self::SqlPool>, std_rng: anansi::server::Rng) -> anansi::web::Result<anansi::web::Response> {
                unimplemented!();
            }
        }
    }
}

impl<D: AsDb> RawRequest<D> {
    fn get_cookies(headers: &HeaderMap<HeaderValue>) -> Result<Cookies> {
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
    pub fn method(&self) -> &Method {
        &self.method
    }
    pub fn to_form_map(&self) -> Result<FormMap> {
        if self.method() == Method::POST {
            if let Some(body) = &self.body {
                let buffer = body.as_slice();
                let mut n = 0;
                let mut map = HashMap::new();
                while n < buffer.len() {
                    let mut m = find_byte(&buffer, n, EQUAL);
                    let key = get_string(&buffer[n..m])?;
                    n = m + 1;
                    m = find_byte(&buffer, n, AMPERSAND);
                    let value = get_string(&buffer[n..m])?;
                    let decoded = percent_decode(&value)?;
                    map.insert(key, decoded);
                    n = m + 1;
                }
                return Ok(FormMap {map})
            }
        }
        Err(WebErrorKind::BadForm.to_box())
    }
    pub async fn new(request: HyperRequest, app_state: Arc<D>, std_rng: Rng) -> Result<Self> {
        let (parts, body) = request.0.into_parts();
        let body = body.collect().await?.to_bytes();
        let body = if !body.is_empty() {
            Some(Body {body: body.to_vec()})
        } else {
            None
        };
        let cookies = Self::get_cookies(&parts.headers)?;
        let params = Parameters::new();
        Ok(Self {
            body,
            method: parts.method,
            url: parts.uri,
            headers: parts.headers,
            cookies,
            params,
            app_state,
            std_rng,
            valid_token: false,
        })
    }
    pub fn headers(&self) -> &HeaderMap {
        &self.headers
    }
    pub fn cookies_mut(&mut self) -> &mut Cookies {
        &mut self.cookies
    }
    pub fn app_state(&self) -> &D {
        &self.app_state
    }
    pub fn pool(&self) -> &<D as AsDb>::SqlDb {
        self.app_state.as_db()
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
    pub fn url(&self) -> &str {
        self.url.path()
    }
    pub fn query(&self) -> Option<&str> {
        self.url.query()
    }
}

pub trait BaseUser: Record<Pk = BigInt> + Send + Sync {
    type Name: fmt::Display;
    type Secret: fmt::Display;
    fn username(&self) -> &Self::Name;
    fn secret(&self) -> &Option<Self::Secret>;
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
