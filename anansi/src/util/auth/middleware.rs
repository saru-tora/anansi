use super::models::{User, BaseRelation};
use super::super::sessions::middleware::Sessions;
use anansi::web::{Result, BaseRequest};

#[async_trait::async_trait]
pub trait Auth: Sessions + BaseRequest {
    async fn auth_admin(&mut self, user: &User) -> Result<()> where Self: Sized {
        use anansi::models::Model;
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
            use anansi::models::Model;
            *self.session_data_mut().get_mut(User::KEY)? = serde_json::json!(user.pk().as_i64());
            self.update_session().await?;
            Ok(())
        } else {
            Err(anansi::db::invalid())
        }
    }
}
