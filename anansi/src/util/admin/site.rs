use anansi::{check, render};
use anansi::web::{View, Response, Result, BaseRequest};
pub use anansi::admin_site::{RecordAdmin, RecordEntry, AdminSite, AdminRef, AdminEntry};
use super::super::auth::admin::{base, Request, AuthAdminView};
use super::super::auth::records::{User, Group};

pub trait HasAdmin: BaseRequest {
    fn admin(&self) -> AdminRef<Self>;
}

pub struct BasicAdminSite<R: Request> {
    registered: Vec<AdminEntry<R>>,
    urls: Vec<(&'static str, View<R>)>,
}

impl<R: Request> AdminSite<R> for BasicAdminSite<R> {
    fn new() -> Self {
        let urls: Vec<(&'static str, View<R>)> = vec![
            ("/admin", Self::index),
            ("/admin/login", AuthAdminView::login),
            ("/admin/logout", AuthAdminView::logout),
        ];
        Self {registered: vec![], urls}
    }
    fn register(&mut self, app_name: String, entry: RecordEntry<R>) {
        for admin_entry in &mut self.registered {
            if admin_entry.app_name() == &app_name {
                admin_entry.entries_mut().push(entry);
                return;
            }
        }
        let entry = AdminEntry::new(app_name, vec![entry]);
        self.registered.push(entry);
    }
    fn admin_entries(&self) -> &Vec<AdminEntry<R>> {
        &self.registered
    }
    fn urls(&self) -> &Vec<(&'static str, View<R>)> {
        &self.urls
    }
    fn urls_mut(&mut self) -> &mut Vec<(&'static str, View<R>)> {
        &mut self.urls
    }
}

impl<R: Request> BasicAdminSite<R> {
    #[check(Group::is_admin)]
    pub async fn index(req: &mut R) -> Result<Response> {
        let title = "Site Administration";
        render!("index")
    }
}
