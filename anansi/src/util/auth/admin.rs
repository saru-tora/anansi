use std::fmt::Debug;
use anansi::models::{Model, ToUrl, FromParams};
use anansi::admin_site::{AdminRef};
use anansi::web::{Result, Response, Reverse, BaseUser, BaseRequest, CsrfDefense, GetModel};
use crate::util::auth;
use anansi::forms::{Form, ToModel, HasModel, ToEdit};
use anansi::{render, redirect, handle, base_view, handle_or_404};
use super::forms::{UserLogin};
use anansi::{raw_check, view_checker, checker, model_admin};
use crate::{_register};
use super::middleware::Auth;
use super::super::sessions::middleware::Sessions;
use super::super::admin::site::{HasAdmin, BasicAdminSite};
use super::models::{User, Group};
use crate::util::admin::site::ModelAdmin;

pub trait Request: BaseRequest + Auth + HasAdmin + Reverse + CsrfDefense + Sessions + GetModel + 'static + Debug {}

view_checker!(if_admin<R: Request>, |req| false, redirect!());

#[base_view]
fn base<R: Request>(req: R) -> Result<Response> {}

model_admin!(auth::models::User {
    form: auth::forms::UserForm,
    add_form: auth::forms::UserNew,
    fields: [username],
});

model_admin!(auth::models::Group {
    form: auth::forms::GroupForm,
    add_form: auth::forms::GroupForm,
    fields: [groupname],
});

pub fn initialize_admin<R: Request>(site: AdminRef<R>) {
    let mut site = site.lock().unwrap();
    _register!(User);
    _register!(Group);
}

#[checker]
impl<R: Request> AuthAdminView<R> {
    #[raw_check(Group::is_visitor)]
    pub async fn login(mut req: R) -> Result<Response> {
        let form = handle!(UserLogin, ToModel<R>, req, user, {
            req.auth_admin(&user).await?;
            req.session().set_and_redirect(&req, BasicAdminSite::index)
        })?.class("cred");
        render!("login")
    }

    #[raw_check(Group::is_admin)]
    pub async fn logout(mut req: R) -> Result<Response> {
        let title = "Log out";
        let form = handle!(req, R, {
            req.session().delete(&req).await?;
            Ok(redirect!())
        })?;
        render!("logout")
    }

    #[raw_check(Group::is_admin)]
    pub async fn model_index<M: ModelAdmin<R> + Send + ToUrl + 'static>(req: R) -> Result<Response>
where <<M as ModelAdmin<R>>::AdminForm as HasModel>::Item: FromParams, <M as Model>::Pk: std::fmt::Display
    {
        let title = M::NAME;
        let models = M::limit(100).query(&req).await?;
        let field_names = <M as ModelAdmin<R>>::field_names();
        let m_edit = Self::model_edit::<M>;
        render!("model_index")
    }

    #[raw_check(Group::is_admin)]
    pub async fn model_new<M: ModelAdmin<R> + 'static>(mut req: R) -> Result<Response>
where <<M as ModelAdmin<R>>::AdminForm as HasModel>::Item: FromParams
    {
        let title = format!("Add {}", M::NAME);
        let form = handle!(<M as ModelAdmin<R>>::AddForm, ToModel<R>, req, || Ok(redirect!(req, BasicAdminSite::index)))?;
        let button = "Create";
        render!("model_new")
    }

    #[raw_check(Group::is_admin)]
    pub async fn model_edit<M: ModelAdmin<R> + 'static>(mut req: R) -> Result<Response>
where <<M as ModelAdmin<R>>::AdminForm as HasModel>::Item: FromParams
    {
        let title = format!("Edit {}", M::NAME);
        let form = handle_or_404!(<M as ModelAdmin<R>>::AdminForm, ToEdit<R>, req, || Ok(redirect!(req, BasicAdminSite::index)))?;
        let button = "Edit";
        render!("model_new")
    }
}
