use crate::admin_page;
use super::admin::{AuthAdminView, Request};
use anansi::web::View;

pub const APP_NAME: &'static str = "auth";

admin_page! {
    ("/admin", AuthAdminView::index),
    ("/admin/login", AuthAdminView::login),
    ("/admin/logout", AuthAdminView::logout),
    ("/admin/group/index", AuthAdminView::group_index),
    ("/admin/group/new", AuthAdminView::group_new),
    ("/admin/user/index", AuthAdminView::user_index),
    ("/admin/user/new", AuthAdminView::user_new),
}
