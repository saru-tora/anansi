use serde_json::Value;
use serde_json::map::Map;

use anansi::server::Rng;
use anansi::web::{Result, BaseRequest, Reverse, RawRequest, View, Response, TokenRef};
use anansi::db::{invalid, DbPool};
use anansi::records::{Record, VarChar, Text, DateTime, Relate, generate_id};
use anansi::{record};

#[record]
#[derive(Debug, Clone)]
pub struct Session {
    pub data: Text,
    pub secret: VarChar<32>,
    pub expires: DateTime,
}

impl<B: BaseRequest> Relate<B> for Session {}

#[derive(Debug, Clone)]
pub struct SessionData {
    data_map: Map<String, Value>,
}

impl SessionData {
    pub fn get(&self, key: &str) -> Result<&Value> {
        self.data_map.get(key).ok_or(invalid())
    }
    pub fn get_mut(&mut self, key: &str) -> Result<&mut Value> {
        self.data_map.get_mut(key).ok_or(invalid())
    }
    pub fn insert(&mut self, k: String, v: Value) -> Option<Value> {
        self.data_map.insert(k, v)
    }
    pub fn to_text(&self) -> Result<Text> {
        Ok(Text::from(serde_json::to_string(&self.data_map)?))
    }
}

fn session_expires() -> DateTime {
    DateTime::after(30*24*60*60)
}

impl Session  {
    pub fn from_guest(rng: &Rng) -> Self {
        Self {
            id: generate_id(),
            data: Text::from(format!("{{\"_user_id\": 0, \"{}\": \"{}\"}}", TokenRef::KEY, rng.new_secret())),
            secret: rng.new_secret(),
            expires: session_expires(),
        }
    }
    pub async fn refresh<B: BaseRequest>(mut self, req: &B) -> Result<Self> {
        self.secret = req.raw().rng().new_secret();
        self.expires = session_expires();
        self.update(req).await?;
        Ok(self)
    }
    pub fn to_data(&self) -> Result<SessionData> {
        Ok(SessionData {data_map: serde_json::from_str(&self.data)?})
    }
    pub async fn gen(pool: &DbPool, rng: &Rng) -> Result<Self> {
        Self::from_guest(rng).raw_save(pool).await
    }
    pub async fn from_raw(raw: &mut RawRequest) -> Result<Self> {
        let st = raw.cookies_mut().remove("st")?;
        let session = Self::whose(session::secret().eq(st)).raw_get(raw.pool()).await?;
        if session.expires > DateTime::now() {
            Ok(session)
        } else {
            Err(invalid())
        }
    }
    pub fn set_and_redirect<B: BaseRequest + Reverse>(&self, req: &B, v: View<B>) -> Result<Response> {
        Ok(Response::redirect(&anansi::url!(req, v)).set_persistent("st", &self.secret, &self.expires))
    }
}
