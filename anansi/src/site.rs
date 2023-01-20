use anansi::web::{Result, BaseRequest};

pub struct Site;

impl Site {
    pub async fn is_visitor<B: BaseRequest>(_req: &B) -> Result<()> {
        Ok(())
    }
}

#[cfg(not(feature = "minimal"))]
use anansi::web::{WebError, WebErrorKind, BaseUser};

#[cfg(not(feature = "minimal"))]
impl Site {
    pub async fn is_auth<B: BaseRequest>(req: &B) -> Result<()> {
        if req.user().is_auth() {
            Ok(())
        } else {
            Err(Box::new(WebError::from(WebErrorKind::Unauthenticated)))
        }
    }
}
