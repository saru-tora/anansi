use std::time::{SystemTime, Duration, UNIX_EPOCH};
use super::BaseCache;
use crate::server::Settings;
use crate::web::{Result, WebErrorKind};

use moka::future::Cache;

#[derive(Clone, Debug)]
pub struct LocalCache(MokaCache);

#[derive(Clone, Debug)]
struct MokaCache {
    default_timeout: Option<usize>,
    cache: Cache<String, Vec<u8>, ahash::RandomState>,
    timeout: Cache<String, usize, ahash::RandomState>,
}

#[async_trait::async_trait]
impl BaseCache for LocalCache {
    async fn new(_settings: &Settings) -> Result<Self> where Self: Sized {
        let default_timeout = Some(300);
        let cache = Cache::builder().max_capacity(10_000).build_with_hasher(ahash::RandomState::default());
        let timeout = Cache::builder().max_capacity(10_000).build_with_hasher(ahash::RandomState::default());
        Ok(Self(MokaCache {default_timeout, cache, timeout} ))
    }
    async fn set(&self, key: &str, value: &[u8]) -> Result<()> {
        self.0.set(key, value).await
    }
    async fn set_ex(&self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()> {
        self.0.set_ex(key, value, timeout).await
    }
    async fn set_many<'a>(&self, items: &'a[(String, Vec<u8>)]) -> Result<()> {
        self.0.set_many(items).await
    }
    async fn get(&self, key: &str) -> Result<Vec<u8>> {
        self.0.get(key).await
    }
    async fn get_many(&self, key: Vec<String>) -> Result<Vec<Vec<u8>>> {
        self.0.get_many(key).await
    }
}

fn now() -> usize {
    SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as usize
}

impl MokaCache {
    async fn set(&self, key: &str, value: &[u8]) -> Result<()> {
        match self.set_ex(key, value, self.default_timeout).await {
            Ok(()) => Ok(()),
            Err(e) => Err(e),
        }
    }
    async fn set_ex(&self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()> {
        self.cache.insert(key.to_string(), value.to_vec()).await;
        if let Some(t) = timeout {
            let cache = self.cache.clone();
            let to = self.timeout.clone();
            let timestamp = now() + t;
            self.timeout.insert(key.to_string(), timestamp).await;
            let key = key.to_string();
            tokio::spawn(async move {
                tokio::time::sleep(Duration::from_secs(t as u64)).await;
                if let Some(t) = to.get(&key) {
                    if t == timestamp {
                        cache.invalidate(&key).await;
                        to.invalidate(&key).await;
                    }
                }
            });
        }
        Ok(())
    }
    async fn set_many<'a>(&self, items: &'a[(String, Vec<u8>)]) -> Result<()> {
        for (key, value) in items {
            self.set(key, value).await?;
        }
        Ok(())
    }
    async fn get(&self, key: &str) -> Result<Vec<u8>> {
        match self.cache.get(key) {
            Some(r) => {
                Ok(r)
            }
            None => {
                Err(WebErrorKind::NoCache.to_box())
            }
        }
    }
    async fn get_many(&self, keys: Vec<String>) -> Result<Vec<Vec<u8>>> {
        let mut v = vec![];
        for key in keys {
            if let Some(val) = self.cache.get(&key) {
                v.push(val);
            } else {
                return Err(WebErrorKind::NoCache.to_box());
            }
        }
        Ok(v)
    }
}
