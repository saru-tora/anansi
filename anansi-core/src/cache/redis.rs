use std::fmt;
use std::sync::Arc;
use super::BaseCache;
use crate::server::Settings;
use crate::web::{Result, WebErrorKind};

use tokio::sync::RwLock;

use redis::AsyncCommands;
use redis::aio::Connection;

#[derive(Clone, Debug)]
pub struct RedisCache(Arc<RwLock<RedisConnection>>);

struct RedisConnection {
    default_timeout: Option<usize>,
    con: Connection,
}

impl fmt::Debug for RedisConnection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Redis connection")
    }
}

#[async_trait::async_trait]
impl BaseCache for RedisCache {
    async fn new(settings: &Settings) -> Result<Self> where Self: Sized {
        let default = settings.get("default").expect("Could not get default cache").as_table().expect("Expected table for default cache");
        let location = default.get("location").expect("Could not get Redis location from settings").as_str().expect("Could not convert Redis location to string");
        let default_timeout = Some(300);
        let client = redis::Client::open(location)?;
        Ok(Self {0: Arc::new(RwLock::new(RedisConnection {default_timeout, con: client.get_async_connection().await?}))})
    }
    async fn set(&self, key: &str, value: &[u8]) -> Result<()> {
        self.0.write().await.set(key, value).await
    }
    async fn set_ex(&self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()> {
        self.0.write().await.set_ex(key, value, timeout).await
    }
    async fn set_many<'a>(&self, items: &'a[(String, Vec<u8>)]) -> Result<()> {
        self.0.write().await.set_many(items).await
    }
    async fn get(&self, key: &str) -> Result<Vec<u8>> {
        self.0.write().await.get(key).await
    }
    async fn get_many(&self, key: Vec<String>) -> Result<Vec<Vec<u8>>> {
        self.0.write().await.get_many(key).await
    }
}

impl RedisConnection {
    async fn set(&mut self, key: &str, value: &[u8]) -> Result<()> {
        match self.set_ex(key, value, self.default_timeout).await {
            Ok(()) => Ok(()),
            Err(e) => Err(e),
        }
    }
    async fn set_ex(&mut self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()> {
        let r = if let Some(t) = timeout {
            self.con.set_ex(key, value, t).await
        } else {
            self.con.set(key, value).await
        };
        match r {
            Ok(()) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
    async fn set_many<'a>(&mut self, items: &'a[(String, Vec<u8>)]) -> Result<()> {
        match self.con.mset_nx(&items).await {
            Ok(()) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
    async fn get(&mut self, key: &str) -> Result<Vec<u8>> {
        match self.con.get::<&str, Vec<u8>>(key).await {
            Ok(r) => {
                if !r.is_empty() {
                  Ok(r)
                } else {
                    Err(WebErrorKind::NoCache.to_box())
                }
            }
            Err(_) => {
                Err(WebErrorKind::NoCache.to_box())
            }
        }
    }
    async fn get_many(&mut self, key: Vec<String>) -> Result<Vec<Vec<u8>>> {
        match self.con.get::<Vec<String>, Vec<Vec<u8>>>(key).await {
            Ok(r) => {
                if !r.is_empty() {
                  Ok(r)
                } else {
                    Err(WebErrorKind::NoCache.to_box())
                }
            }
            Err(e) => {
                Err(Box::new(e))
            }
        }
    }
}
