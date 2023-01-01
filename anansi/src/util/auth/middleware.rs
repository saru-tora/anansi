use super::records::{User, BaseRelation};
use super::super::sessions::middleware::Sessions;
use anansi::web::{Result, BaseRequest};
use anansi::db::invalid;
use totp_rs::{Algorithm, TOTP, Secret};

#[async_trait::async_trait]
pub trait Auth: Sessions + BaseRequest {
    async fn auth_admin(&mut self, user: &User) -> Result<()> where Self: Sized {
        use anansi::records::Record;
        *self.user_mut().pk_mut() = user.pk();
        if BaseRelation::check("auth_group", 1, "member", self).await.is_ok() {
            *self.session_data_mut().get_mut(User::KEY)? = serde_json::json!(user.pk().as_i64());
            self.update_session().await?;
            Ok(())
        } else {
            Err(anansi::db::invalid())
        }
    }
    async fn auth(&mut self, user: &User) -> Result<()> {
        use anansi::web::BaseUser;
        if user.is_auth() {
            use anansi::records::Record;
            *self.session_data_mut().get_mut(User::KEY)? = serde_json::json!(user.pk().as_i64());
            self.update_session().await?;
            Ok(())
        } else {
            Err(anansi::db::invalid())
        }
    }
    fn new_totp(&mut self, issuer: Option<String>) -> Result<String> {
        use anansi::web::BaseUser;
        let secret = self.raw().rng().new_secret();
        let ss = secret.to_string();
        self.session_data_mut().insert(User::TOTP_KEY.to_string(), serde_json::json!(secret));
        let totp = TOTP::new(
            Algorithm::SHA1,
            6,
            1,
            30,
            Secret::Raw(ss.as_bytes().to_vec()).to_bytes().unwrap(),
            issuer,
            self.user().username().to_string(),
        )?;
        match totp.get_qr() {
            Ok(o) => Ok(o),
            Err(_) => Err(invalid()),
        }
    }
    fn temp_totp(&self) -> Result<String> {
        Ok(self.session_data().get(User::TOTP_KEY)?.to_string())
    }
}
