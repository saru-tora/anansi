use anansi::web::prelude::*;

routes! {
    path!("/", anansi::util::auth::start::views::StartView::start),
}
