#[cfg(debug_assertions)]
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(debug_assertions)]
#[anansi::viewer]
impl<B: anansi::web::BaseRequest + 'static> StartView<B> {
    #[anansi::check(anansi::site::Site::is_visitor)]
    pub async fn start(_req: &mut B) -> anansi::web::Result<anansi::web::Response> {
        anansi::render!("start")
    }
}
