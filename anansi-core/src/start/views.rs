use anansi::{viewer, render};
use crate::web::{if_guest, Result, Response, BaseRequest};

#[cfg(debug_assertions)]
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(debug_assertions)]
use anansi::check;

#[cfg(debug_assertions)]
#[viewer]
impl<B: BaseRequest + 'static> StartView<B> {
    #[check(if_guest)]
    async fn start<B: BaseRequest + 'static>(_req: B) -> Result<Response> {
        render!("start")
    }
}
