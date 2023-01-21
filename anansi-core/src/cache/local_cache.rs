use std::time::{SystemTime, UNIX_EPOCH};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use super::BaseCache;
use crate::server::Settings;
use crate::web::{Result, WebErrorKind};

#[derive(Debug)]
struct LocalValue {
    index: usize,
    expires: Option<usize>,
}

impl LocalValue {
    fn new(index: usize, expires: Option<usize>) -> Self {
        Self {index, expires}
    }
}

#[derive(Debug)]
struct Entry {
    val: Vec<u8>,
    prev: usize,
    next: usize,
}

#[derive(Clone, Debug)]
pub struct LocalCache(Arc<RwLock<Lru>>);

#[derive(Debug)]
struct Lru {
    default_timeout: Option<usize>,
    length: usize,
    head: usize,
    tail: usize,
    entries: Vec<Entry>,
    storage: HashMap<String, LocalValue>,
}

impl Lru {
    async fn set(&mut self, key: &str, value: &[u8]) -> Result<()> {
        self.set_ex(key, value, self.default_timeout).await
    }
    async fn set_ex(&mut self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()> {
        let index = self.insert(value.to_vec());
        let t = if let Some(to) = timeout {
            Some(to + now())
        } else {
            None
        };
        let lv = LocalValue::new(index, t);
        self.storage.insert(key.to_string(), lv);
        Ok(())
    }
    async fn get(&mut self, key: &str) -> Result<Vec<u8>> {
        let mut change = false;
        let index = if let Some(lv) = self.storage.get(key) {
            if let Some(t) = lv.expires {
                let n = now();
                if t >= n {
                    change = true;
                }
                lv.index
            } else {
                return Err(WebErrorKind::NoCache.to_box());
            }
        } else {
            return Err(WebErrorKind::NoCache.to_box());
        };
        if change {
            self.touch(index);
            let e = &self.entries[index];
            return Ok(e.val.clone());
        } else {
            self.remove(index);
            self.storage.remove(key);
            return Err(WebErrorKind::NoCache.to_box());
        }
    }
    fn push_front(&mut self, index: usize) {
        if self.entries.len() == 1 {
            self.tail = index;
        } else {
            self.entries[index].next = self.head;
            self.entries[self.head].prev = index;
        }
    }
    fn pop_back(&mut self) -> usize {
        let old_tail = self.tail;
        let new_tail = self.entries[old_tail].prev;
        self.tail = new_tail;
        old_tail
    }
    fn remove(&mut self, index: usize) {
        assert!(self.length > 0);

        let prev = self.entries[index].prev;
        let next = self.entries[index].next;

        if index == self.head {
            self.head = next;
        } else {
            self.entries[prev].next = next;
        }

        if index == self.tail {
            self.tail = prev;
        } else {
            self.entries[next].prev = prev;
        }

        self.length -= 1;
    }
    fn touch(&mut self, index: usize) {
        if index != self.head {
            self.remove(index);
            self.length += 1;
            self.push_front(index);
        }
    }
    fn insert(&mut self, val: Vec<u8>) -> usize {
        let entry = Entry {val, prev: 0, next: 0};

        let new_head = if self.length == self.entries.capacity() {
            let last_index = self.pop_back();
            self.entries[last_index] = entry;
            last_index
        } else {
            self.entries.push(entry);
            self.length += 1;
            self.entries.len() - 1
        };

        self.push_front(new_head);
        new_head
    }
}

fn now() -> usize {
    SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as usize
}

#[async_trait::async_trait]
impl BaseCache for LocalCache {
    async fn new(_settings: &Settings) -> Result<Self> {
        let default_timeout = Some(300);
        Ok(Self {0: Arc::new(RwLock::new(Lru {default_timeout, length: 0, head: 0, tail: 0, entries: Vec::with_capacity(300), storage: HashMap::with_capacity(300)}))})
    }
    async fn set(&self, key: &str, value: &[u8]) -> Result<()> {
        self.0.write().await.set(key, value).await
    }
    async fn set_ex(&self, key: &str, value: &[u8], timeout: Option<usize>) -> Result<()> {
        self.0.write().await.set_ex(key, value, timeout).await
    }
    async fn get(&self, key: &str) -> Result<Vec<u8>> {
        self.0.write().await.get(key).await
    }
}
