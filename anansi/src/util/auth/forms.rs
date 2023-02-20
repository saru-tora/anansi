use async_trait::async_trait;
use anansi::{form, form_error, err_box, GetData, ToEdit};
use anansi::web::{WebErrorKind, BaseRequest, Result};
use anansi::records::{Record, DateTime};
use anansi::forms::{Form, Field, VarChar, Text, FormError, ToRecord};
use super::records::{User, user::username, Group, Filter, hash_password};
use super::admin::Request;
use crate::util::auth::middleware::Auth;

#[form(User)]
pub struct UserLogin {
    pub username: VarChar<150>,
    #[field(widget = "Password")]
    pub password: VarChar<150>,
}

#[async_trait]
impl<B: BaseRequest> ToRecord<B> for UserLogin {
    async fn on_post(&mut self, data: UserLoginData, req: &mut B) -> Result<User> {
        if let Ok(user) = User::whose(username().eq(&data.username)).get(req).await {
            if user.verify(&data.password).is_ok() {
                return Ok(user);
            }
        }
        form_error!("Problem logging in.")
    }
}

#[form(User)]
pub struct UserTotp {
    pub code: Text,
}

#[async_trait]
impl<B: BaseRequest + Auth> ToRecord<B> for UserTotp {
    async fn on_post(&mut self, data: UserTotpData, req: &mut B) -> Result<User> {
        if let Ok(secret) = req.temp_totp() {
            use anansi::web::BaseUser;
            let name = req.user().username().to_string();
            if let Ok(mut user) = User::whose(username().eq(name)).get(req).await {
                user.secret = Some(anansi::records::Text::from(secret));
                if user.set_totp(None, data.code.to_string()).is_ok() {
                    return match user.save(req).await {
                        Ok(_) => Ok(user),
                        Err(_) => err_box!(FormError::new("Problem adding totp.")),
                    };
                }
            }
        }
        form_error!("Problem verifying code.")
    }
}

#[form(User)]
pub struct UserNew {
    pub username: VarChar<150>,
    #[field(required = "false")]
    pub email: Text,
    #[field(widget = "Password")]
    pub password: VarChar<150>,
    #[field(widget = "Password")]
    pub confirm: VarChar<150>,
}

impl UserNew {
    pub fn clean(data: &<Self as Form>::Data) -> Result<()> {
        if data.password == &data.confirm {
            Ok(())
        } else {
            Err(WebErrorKind::BadPassword.to_box())
        }
    }
}

#[async_trait]
impl<B: BaseRequest> ToRecord<B> for UserNew {
    async fn on_post(&mut self, data: UserNewData, req: &mut B) -> Result<User> {
        let clean_name = data.username.as_str();
        let clean_name = match User::validate_username(clean_name, req.raw().pool()).await {
            Ok(username) => username,
            Err(feedback) => {
                self.username.add_error(Box::new(FormError::new(feedback.warning())));
                self.username.add_error(Box::new(FormError::new(feedback.suggestion())));
                match feedback.into_username() {
                    Some(username) => username,
                    None => return Err(WebErrorKind::BadUsername.to_box()),
                }
            },
        };
        if Self::clean(&data).is_err() {
            self.confirm.add_error(Box::new(FormError::new("Passwords do not match.")));
        }
        let entropy = User::check_password(&data.password);
        if let Some(feedback) = entropy.feedback() {
            self.password.add_error(Box::new(FormError::new(&feedback.warning().to_string())));
            self.password.add_error(Box::new(FormError::new(feedback.suggestion())));
        }
        self.check_field_errors()?;
        let password = hash_password(&data.password)?;
        let now = DateTime::now();
        let user = User::new()
            .username(clean_name)
            .email(data.email)
            .password(password)
            .last_login(now)
            .date_joined(now);
        match user.saved(req).await {
            Ok(u) => Ok(u),
            Err(_) => err_box!(FormError::new("Problem adding user.")),
        }
    }
}

#[form(User)]
#[derive(GetData, ToEdit)]
pub struct UserForm {
    pub username: VarChar<150>,
}

#[form(Group)]
#[derive(GetData, ToEdit)]
pub struct GroupForm {
    pub groupname: VarChar<150>,
}

#[async_trait]
impl<B: Request> ToRecord<B> for GroupForm {
    async fn on_post(&mut self, data: GroupFormData, req: &mut B) -> Result<Group> {
        let group = Group::new()
            .groupname(data.groupname);
        match group.saved(req).await {
            Ok(g) => Ok(g),
            Err(_) => err_box!(FormError::new("Problem adding group.")),
        }
    }
}

#[form(Filter)]
#[derive(GetData, ToEdit)]
pub struct FilterForm {
    pub filter_name: Text,
    pub filter: Text,
}

#[async_trait]
impl<B: Request> ToRecord<B> for FilterForm {
    async fn on_post(&mut self, data: FilterFormData, req: &mut B) -> Result<Filter> {
        let table_name = req.params().get("table_name")?.parse()?;
        let raw_query = req.params().get("raw_query")?.parse()?;
        let filter = Filter::new()
            .table_name(table_name)
            .filter_name(data.filter_name)
            .filter(data.filter)
            .raw_query(raw_query)
            .saved(req).await?;
        Ok(filter)
    }
}

#[form]
pub struct AdminSearch {
    #[field(required = "false")]
    pub q: VarChar<150>,
}
