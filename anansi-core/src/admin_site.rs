use std::sync::{Arc, Mutex};
use std::fmt;//::{self, Display};
use anansi::web::{BaseRequest, View, ParamsToModel};
use anansi::models::{Model, FromParams};
use anansi::forms::{Form, ToModel, ToEdit, HasModel};

pub type AdminRef<B> = Arc<Mutex<dyn AdminSite<B>>>;

pub trait AdminSite<B: BaseRequest>: Sync + Send {
    fn new() -> Self where Self: Sized;
    fn register(&mut self, app_name: String, entry: ModelEntry<B>);
    fn admin_entries(&self) -> &Vec<AdminEntry<B>>;
    fn urls(&self) -> &Vec<(&'static str, View<B>)>;
    fn urls_mut(&mut self) -> &mut Vec<(&'static str, View<B>)>;
}

impl<B: BaseRequest> fmt::Debug for dyn AdminSite<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AdminSite")
    }
}

pub struct ModelEntry<B: BaseRequest> {
    pub name: &'static str,
    pub index: View<B>,
    pub new: View<B>,
}

pub struct AdminEntry<B: BaseRequest> {
    app_name: String,
    entries: Vec<ModelEntry<B>>,
}

impl<B: BaseRequest> AdminEntry<B> {
    pub fn new(app_name: String, entries: Vec<ModelEntry<B>>) -> Self {
        Self {app_name, entries}
    }
    pub fn app_name(&self) -> &String {
        &self.app_name
    }
    pub fn entries(&self) -> &Vec<ModelEntry<B>> {
        &self.entries
    }
    pub fn entries_mut(&mut self) -> &mut Vec<ModelEntry<B>> {
        &mut self.entries
    }
}

#[async_trait::async_trait]
pub trait ModelAdmin<B: BaseRequest + ParamsToModel>: Model
where <<Self as ModelAdmin<B>>::AdminForm as HasModel>::Item: FromParams
{
    type AdminForm: Form + Send + ToEdit<B>;
    type AddForm: Form + Send + ToModel<B>;

    fn field_names() -> &'static [&'static str];

    async fn fields(self, req: &B) -> Vec<String>;
}
