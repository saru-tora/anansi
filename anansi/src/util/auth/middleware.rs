use super::models::User;
use super::super::sessions::middleware::Sessions;
use anansi::web::Result;

#[async_trait::async_trait]
pub trait Auth: Sessions {
    fn user(&self) -> &User;
    async fn auth_admin(&mut self, user: &User) -> Result<()> {
        if user.is_admin() {
            use anansi::models::Model;
            *self.session_data_mut().get_mut(User::KEY)? = serde_json::json!(user.pk().as_i64());
            self.update_session().await?;
            Ok(())
        } else {
            Err(anansi::db::invalid())
        }
    }
    async fn auth(&mut self, user: &User) -> Result<()> {
        if user.is_auth() {
            use anansi::models::Model;
            *self.session_data_mut().get_mut(User::KEY)? = serde_json::json!(user.pk().as_i64());
            self.update_session().await?;
            Ok(())
        } else {
            Err(anansi::db::invalid())
        }
    }
    fn check_admin(&self) -> bool {
        self.user().is_admin()
    }
    fn check_auth(&self) -> bool {
        self.user().is_auth()
    }
}
