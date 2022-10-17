use async_trait::async_trait;
use anansi::{form, form_error, err_box};
use anansi::web::{BaseRequest, Result, FormMap};
use anansi::db::invalid;
use anansi::models::{Model};
use anansi::forms::{Form, Field, VarChar, FormError, ToModel, GetData, ToEdit};
use super::models::{User, user::username, Group, hash_password};
use super::admin::Request;

#[form(User)]
pub struct UserLogin {
    pub username: VarChar<150>,
    #[field(widget = "Password")]
    pub password: VarChar<150>,
}

#[async_trait]
impl<B: BaseRequest> ToModel<B> for UserLogin {
    async fn on_post(&mut self, data: UserLoginData, req: &B) -> Result<User> {
        if let Ok(user) = User::whose(username().eq(&data.username)).get(req).await {
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
                self.username.add_error(Box::new(FormError::new(feedback.suggestion())));
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
        User::new(clean_name, password).save(req).await.or(err_box!(FormError::new("Problem adding user.")))
    }
}

#[form(User)]
pub struct UserForm {
    pub username: VarChar<150>,
}

#[async_trait]
impl<R: Request> GetData<R> for UserForm {
    fn from_map(form_map: FormMap) -> Result<UserFormData> {
        Ok(UserFormData::new(form_map.get("username")?.parse()?))
    }
    async fn from_model(user: User, _req: &R) -> Result<UserFormData> {
        Ok(UserFormData::new(user.username))
    }
}

#[async_trait]
impl<R: Request> ToEdit<R> for UserForm {
    async fn on_post(&mut self, data: UserFormData, req: &R) -> Result<User> {
        let mut user: User = req.get_model().await?;
        user.username = data.username;
        user.update(req).await?;
        Ok(user)
    }
}

#[form(Group)]
pub struct GroupForm {
    pub groupname: VarChar<150>,
}

#[async_trait]
impl<B: Request> ToModel<B> for GroupForm {
    async fn on_post(&mut self, data: GroupFormData, req: &B) -> Result<Group> {
        Group::new(data.groupname).save(req).await
            .or(err_box!(FormError::new("Problem adding group.")))
    }
}

#[async_trait]
impl<R: Request> GetData<R> for GroupForm {
    fn from_map(form_map: FormMap) -> Result<GroupFormData> {
        Ok(GroupFormData::new(form_map.get("name")?.parse()?))
    }
    async fn from_model(group: Group, _req: &R) -> Result<GroupFormData> {
        Ok(GroupFormData::new(group.groupname))
    }
}

#[async_trait]
impl<R: Request> ToEdit<R> for GroupForm {
    async fn on_post(&mut self, data: GroupFormData, req: &R) -> Result<Group> {
        let mut group: Group = req.get_model().await?;
        group.groupname = data.groupname;
        group.update(req).await?;
        Ok(group)
    }
}
