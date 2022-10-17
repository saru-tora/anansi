use anansi::{raw_check, checker, render};
use anansi::web::{Result, Response};
use anansi::util::auth::models::Group;
use crate::project::Request;

#[checker]
impl<R: Request> ErrorView<R> {
    #[raw_check(Group::is_visitor)]
    pub async fn not_found(_req: R) -> Result<Response> {
        render!("not_found")
    }
}
