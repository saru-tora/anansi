use anansi_macros::form;
use crate::forms::{ToEmpty};
use crate::web::BaseRequest;

#[form]
pub struct EmptyForm {}

#[async_trait::async_trait]
impl<B: BaseRequest> ToEmpty<B> for EmptyForm {}
