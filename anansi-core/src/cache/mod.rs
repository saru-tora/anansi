use crate::server::Settings;
use crate::web::Result;

pub mod local_cache;

#[cfg(feature = "redis")]
pub mod redis;

pub mod prelude {
    pub use anansi::{cache, cacheable};
    pub use super::BaseCache;
}

#[macro_export]
macro_rules! app_cache {
    (local) => {
        pub type AppCache = anansi::cache::local_cache::LocalCache;
    };
    (redis) => {
        pub type AppCache = anansi::cache::redis::RedisCache;
    };
}

#[macro_export]
macro_rules! cache {
    ($req:ident, $key:expr, $e:expr) => {
            anansi::_cache!($req, $key, $e, {
                let e = $e;
                $req.cache_mut().set($key, &serde_json::to_vec(&e)?).await?;
                e
            })
    };
    ($req:ident, $to:expr, $key:expr, $e:expr) => {
            anansi::_cache!($req, $key, $e, {
                let e = $e;
                $req.cache_mut().set_ex($key, &serde_json::to_vec(&e)?, $to).await?;
                e
            })
    };
}

#[macro_export]
macro_rules! _cache {
    ($req:ident, $key:expr, $e:expr, $c:block) => {
        if let Ok(v) = $req.cache().get($key).await {
            match serde_json::from_slice(&v) {
                Ok(o) => o,
                Err(e) => {
                    return Err(Box::new(e));
                }
            }
        } else {
            $c
        }
    }
}

pub trait Cacheable {
    fn to_bytes(&self) -> Vec<u8>;
    fn from_bytes(bytes: Vec<u8>) -> Result<Self> where Self: Sized;
}

#[async_trait::async_trait]
pub trait BaseCache: Clone + Send + Sync {
    async fn new(settings: &Settings) -> Result<Self> where Self: Sized;
    async fn set(&self, key: &str, value: &[u8]) -> Result<()>;
    async fn set_many<'a>(&self, items: &'a[(String, Vec<u8>)]) -> Result<()>;
    async fn set_ex(&self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()>;
    async fn get(&self, key: &str) -> Result<Vec<u8>>;
    async fn get_many(&self, keys: Vec<String>) -> Result<Vec<Vec<u8>>>;
}
