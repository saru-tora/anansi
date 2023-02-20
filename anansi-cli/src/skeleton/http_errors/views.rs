use anansi::{check, viewer, render};
use anansi::web::{Result, Response};
use anansi::site::Site;
use crate::project::Request;

#[viewer]
impl<R: Request> ErrorView<R> {
    #[check(Site::is_visitor)]
    pub async fn not_found(_req: &mut R) -> Result<Response> {
        Ok(Response::new(404, raw_render!("not_found")))
    }
}
