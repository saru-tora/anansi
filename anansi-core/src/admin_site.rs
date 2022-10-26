use std::sync::{Arc, Mutex};
use std::fmt;
use anansi::db::Whose;
use anansi::web::{BaseRequest, View, GetRecord};
use anansi::records::{Record, FromParams};
use anansi::forms::{Form, ToRecord, ToEdit, HasRecord};

pub type AdminRef<B> = Arc<Mutex<dyn AdminSite<B>>>;

pub trait AdminSite<B: BaseRequest>: Sync + Send {
    fn new() -> Self where Self: Sized;
    fn register(&mut self, app_name: String, entry: RecordEntry<B>);
    fn admin_entries(&self) -> &Vec<AdminEntry<B>>;
    fn urls(&self) -> &Vec<(&'static str, View<B>)>;
    fn urls_mut(&mut self) -> &mut Vec<(&'static str, View<B>)>;
}

impl<B: BaseRequest> fmt::Debug for dyn AdminSite<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AdminSite")
    }
}

pub struct RecordEntry<B: BaseRequest> {
    pub name: &'static str,
    pub index: View<B>,
    pub new: View<B>,
}

pub struct AdminEntry<B: BaseRequest> {
    app_name: String,
    entries: Vec<RecordEntry<B>>,
}

impl<B: BaseRequest> AdminEntry<B> {
    pub fn new(app_name: String, entries: Vec<RecordEntry<B>>) -> Self {
        Self {app_name, entries}
    }
    pub fn app_name(&self) -> &String {
        &self.app_name
    }
    pub fn entries(&self) -> &Vec<RecordEntry<B>> {
        &self.entries
    }
    pub fn entries_mut(&mut self) -> &mut Vec<RecordEntry<B>> {
        &mut self.entries
    }
}

pub trait AdminField {
    fn admin_field(&self) -> String;
}

impl<D: fmt::Display> AdminField for Option<D> {
    fn admin_field(&self) -> String {
        if let Some(s) = self {
            s.to_string()
        } else {
            String::new()
        }
    }
}

#[async_trait::async_trait]
pub trait RecordAdmin<B: BaseRequest + GetRecord>: Record
where <<Self as RecordAdmin<B>>::AdminForm as HasRecord>::Item: FromParams
{
    type AdminForm: Form + Send + ToEdit<B>;
    type AddForm: Form + Send + ToRecord<B>;

    fn field_names() -> &'static [&'static str];

    async fn fields(self, req: &B) -> Vec<String>;

    fn searchable() -> bool {
        false
    }

    fn search(_terms: &Vec<String>) -> Whose<Self> {
        unimplemented!()
    }
}
