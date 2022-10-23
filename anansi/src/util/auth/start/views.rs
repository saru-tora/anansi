#[cfg(debug_assertions)]
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(debug_assertions)]
#[anansi::record_view]
impl<B: anansi::web::BaseRequest + 'static> StartView<B> {
    #[anansi::check(super::super::records::Group::is_visitor)]
    pub async fn start(_req: B) -> anansi::web::Result<anansi::web::Response> {
        anansi::render!("start")
    }
}
