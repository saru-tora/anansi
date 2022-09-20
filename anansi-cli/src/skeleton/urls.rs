use anansi::web::prelude::*;

routes! {
    path("/", anansi::start::views::StartView::start),
}
