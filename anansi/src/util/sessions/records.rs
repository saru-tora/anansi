use serde_json::Value;
use serde_json::map::Map;

use anansi::server::Rng;
use anansi::web::{Result, BaseRequest, Reverse, RawRequest, View, Response, TokenRef, WebErrorKind};
use anansi::db::DbPool;
use anansi::records::{Record, VarChar, Text, DateTime, Relate, generate_id};
use anansi::record;

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
        if let Some(v) = self.data_map.get(key) {
            Ok(v)
        } else {
            Err(WebErrorKind::NoData.to_box())
        }
    }
    pub fn get_mut(&mut self, key: &str) -> Result<&mut Value> {
        if let Some(v) = self.data_map.get_mut(key) {
            Ok(v)
        } else {
            Err(WebErrorKind::NoData.to_box())
        }
    }
    pub fn insert(&mut self, k: String, v: Value) -> Option<Value> {
        self.data_map.insert(k, v)
    }
    pub fn to_text(&self) -> Result<Text> {
        Ok(Text::from(serde_json::to_string(&self.data_map)?))
    }
}

fn session_expires() -> Result<DateTime> {
    if let Some(datetime) = DateTime::after(30*24*60*60) {
        Ok(datetime)
    } else {
        Err(WebErrorKind::BadDateTime.to_box())
    }
}

impl Session  {
    pub fn test() -> Result<Self> {
        Ok(Self {
            id: generate_id(),
            data: Text::from(format!("{{\"_user_id\": 0, \"{}\": \"{}\"}}", TokenRef::KEY, 0)),
            secret: VarChar::new(),
            expires: session_expires()?,
        })
    }
    pub fn from_guest(rng: &Rng) -> Result<Self> {
        Ok(Self {
            id: generate_id(),
            data: Text::from(format!("{{\"_user_id\": 0, \"{}\": \"{}\"}}", TokenRef::KEY, rng.new_secret())),
            secret: rng.new_secret(),
            expires: session_expires()?,
        })
    }
    pub async fn refresh<B: BaseRequest>(mut self, req: &B) -> Result<Self> {
        self.secret = req.raw().rng().new_secret();
        self.expires = session_expires()?;
        self.update(req).await?;
        Ok(self)
    }
    pub fn to_data(&self) -> Result<SessionData> {
        Ok(SessionData {data_map: serde_json::from_str(&self.data)?})
    }
    pub async fn gen<D: DbPool>(pool: &D, rng: &Rng) -> Result<Self> {
        let session = Self::from_guest(rng)?;
        session.raw_save(pool).await?;
        Ok(session)
    }
    pub async fn from_raw<D: DbPool>(raw: &mut RawRequest<D>) -> Result<Self> {
        let st = raw.cookies_mut().remove("st")?;
        let session = Self::whose(session::secret().eq(st)).raw_get(raw.pool()).await;
        let session = match session {
            Ok(s) => s,
            Err(e) => {
                return Err(e);
            }
        };
        if session.expires > DateTime::now() {
            Ok(session)
        } else {
            Err(WebErrorKind::ExpiredSession.to_box())
        }
    }
    pub fn set_and_redirect<B: BaseRequest + Reverse>(&self, req: &B, v: View<B>) -> Result<Response> {
        Ok(Response::redirect(&anansi::url!(req, v)).set_persistent("st", &self.secret, &self.expires))
    }
}
