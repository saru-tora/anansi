use crate::settings::prelude::*;

#[viewer]
impl<R: Request> Viewer<R> {
    #[view(if_guest)]
    pub fn index(req: &Request) -> Result<Response> {
        let title = "Title";
    }
}
