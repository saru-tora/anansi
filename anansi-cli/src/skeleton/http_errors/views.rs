use anansi::{check, record_view, render};
use anansi::web::{Result, Response};
use anansi::util::auth::records::Group;
use crate::project::Request;

#[record_view]
impl<R: Request> ErrorView<R> {
    #[check(Group::is_visitor)]
    pub async fn not_found(_req: R) -> Result<Response> {
        render!("not_found")
    }
}
