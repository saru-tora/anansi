use async_trait::async_trait;
use anansi::{form, form_error, err_box};
use anansi::web::{BaseRequest, Result};
use anansi::db::invalid;
use anansi::models::{Model, DataType, Boolean};
use anansi::forms::{Form, Field, VarChar, FormError, ToModel};
use super::models::{User, user::username, Group, hash_password};

#[form(User)]
pub struct UserLogin {
    pub username: VarChar<150>,
    #[field(widget = "Password")]
    pub password: VarChar<150>,
}

#[async_trait]
impl<B: BaseRequest> ToModel<B> for UserLogin {
    async fn on_post(&mut self, data: UserLoginData, req: &B) -> Result<User> {
        if let Ok(user) = User::whose(username().eq().data_ref(&data.username)).get(req).await {
            if user.verify(&data.password).is_ok() {
                return Ok(user);
            }
        }
        form_error!("Problem logging in.")
    }
}

#[form(User)]
pub struct UserNew {
    pub username: VarChar<150>,
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
            Err(invalid())
        }
    }
}

#[async_trait]
impl<B: BaseRequest> ToModel<B> for UserNew {
    async fn on_post(&mut self, data: UserNewData, req: &B) -> Result<User> {
        let clean_name = data.username.as_str();
        let clean_name = match User::validate_username(clean_name, req.raw().pool()).await {
            Ok(username) => username,
            Err(feedback) => {
                self.username.add_error(Box::new(FormError::new(feedback.warning())));
                match feedback.into_username() {
                    Some(username) => username,
                    None => return Err(invalid()),
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
        User::new(clean_name, password, Boolean::from_val(false).unwrap()).save(req).await.or(err_box!(FormError::new("Problem adding user.")))
    }
}

#[form(Group)]
pub struct GroupNew {
    pub name: VarChar<150>,
}

#[async_trait]
impl<B: BaseRequest> ToModel<B> for GroupNew {
    async fn on_post(&mut self, data: GroupNewData, req: &B) -> Result<Group> {
        Group::new(data.name).save(req).await
            .or(err_box!(FormError::new("Problem adding group.")))
    }
}
