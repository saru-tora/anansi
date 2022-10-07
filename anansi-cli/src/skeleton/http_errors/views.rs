use anansi::{check, viewer, render};
use anansi::web::{if_guest, Result, Response};
use crate::project::Request;

#[viewer]
impl<R: Request> ErrorView<R> {
    #[check(if_guest)]
    pub async fn not_found(_req: R) -> Result<Response> {
        render!("not_found")
    }
}
