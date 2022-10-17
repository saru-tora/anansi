use anansi::{checker, render};
use anansi::web::{Result, Response, BaseRequest};
use super::super::models::Group;

#[cfg(debug_assertions)]
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(debug_assertions)]
use anansi::raw_check;

#[cfg(debug_assertions)]
#[checker]
impl<B: BaseRequest + 'static> StartView<B> {
    #[raw_check(Group::is_visitor)]
    pub async fn start(_req: B) -> Result<Response> {
        render!("start")
    }
}
