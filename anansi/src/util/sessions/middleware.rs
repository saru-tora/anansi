use anansi::web::Result;
use super::models::{Session, SessionData};

#[async_trait::async_trait]
pub trait Sessions {
    fn session(&self) -> &Session;
    fn session_data(&self) -> &SessionData;
    fn session_data_mut(&mut self) -> &mut SessionData;
    async fn update_session(&mut self) -> Result<()>;
}
