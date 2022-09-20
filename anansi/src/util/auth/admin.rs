use anansi::models::Model;
use anansi::web::{Result, Response, Reverse, BaseRequest, CsrfDefense, if_guest};
use super::models::{user, User, group, Group};
use anansi::forms::{Form, Field, ToModel};
use anansi::{render, redirect, handle};
use super::forms::{UserNew, UserLogin, GroupNew};
use anansi::{check, checker, viewer};
use super::middleware::Auth;
use super::super::sessions::middleware::Sessions;

pub trait Request: BaseRequest + Auth + Reverse + CsrfDefense + Sessions + 'static {}

checker!(if_admin<R: Request>, |req| req.check_admin(), redirect!());

#[viewer]
impl<R: Request> AuthAdminView<R> {
    #[check(if_admin)]
    async fn index(req: R) -> Result<Response> {
        let title = "Site Administration";
        render!("index")
    }

    #[check(if_guest)]
    async fn login(mut req: R) -> Result<Response> {
        let form = handle!(UserLogin, ToModel<R>, req, user, {
            req.auth_admin(&user).await?;
            req.session().set_and_redirect(&req, Self::index)
        })?.class("cred");
        render!("login")
    }

    #[check(if_admin)]
    async fn logout(mut req: R) -> Result<Response> {
        let title = "Log out";
        let form = handle!(req, R, {
            req.session().delete(&req).await?;
            Ok(redirect!())
        })?;
        render!("logout")
    }

    #[check(if_admin)]
    async fn group_index(req: R) -> Result<Response> {
        let title = "Groups";
        let groups = Group::order_by(group::id().desc()).limit(5).query(&req).await?;
        render!("group_index")
    }

    #[check(if_admin)]
    async fn user_index(req: R) -> Result<Response> {
        let title = "Users";
        let users = User::order_by(user::id().desc()).limit(5).query(&req).await?;
        render!("user_index")
    }

    #[check(if_admin)]
    async fn group_new(mut req: R) -> Result<Response> {
        let title = "Add Group";
        let form = handle!(GroupNew, ToModel<R>, req, || Ok(redirect!(req, AuthAdminView::index)))?;
        render!("group_new")
    }

    #[check(if_admin)]
    async fn user_new(mut req: R) -> Result<Response> {
        let title = "Add User";
        let form = handle!(UserNew, ToModel<R>, req, _a, Ok(redirect!(req, AuthAdminView::index)))?;
        render!("user_new")
    }
}
