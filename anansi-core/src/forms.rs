use async_trait::async_trait;

use std::fmt;
use std::collections::HashMap;
use std::error::Error;

use crate::web::{Reverse, TokenRef, CsrfDefense, Result, View, FormMap, BaseRequest, ParamsToModel};
use crate::db::invalid;
use crate::models::{Model, FromParams};
pub use crate::empty::EmptyForm;

#[macro_export]
macro_rules! handle {
    ($req:ident, $t:ident, $b:expr) => {
        anansi::_handle!(anansi::forms::EmptyForm, anansi::forms::ToEmpty<$t>, $req, _a, async {$b}.await)
    };
    ($form:ty, $trait:ty, $req:ident, |$post:ident| $b:expr) => {
        anansi::_handle!($form, $trait, $req, $post, $b)
    };
    ($form:ty, $trait:ty, $req:ident, || $b:expr) => {
        anansi::_handle!($form, $trait, $req, _a, $b)
    };
    ($form:ty, $trait:ty, $req:ident, $post:ident, $b:expr) => {
        anansi::_handle!($form, $trait, $req, $post, async {$b}.await)
    };
}

#[macro_export]
macro_rules! handle_or_404 {
    ($form:ty, $trait:ty, $req:ident, |$post:ident| $b:expr) => {
        anansi::_handle_or_404!($form, $trait, $req, $post, $b)
    };
    ($form:ty, $trait:ty, $req:ident, $post:ident, $b:expr) => {
        anansi::_handle!($form, $trait, $req, $post, async {$b}.await)
    };
}

#[macro_export]
macro_rules! _handle {
    ($form:ty, $trait:ty, $req:ident, $post:ident, $b:expr) => {
        anansi::base_handle! {$form, $trait, $req, $post, $b,
            {
                use anansi::forms::Form;
                let mut form = <$form as $trait>::on_get(&$req).await?;
                form.post(&$req.token()?);
                Ok(form)
            },
            {
                use anansi::forms::Form;
                let res = <$form>::from_post(&mut $req);
                if let Ok(mut form) = res {
                    if let Ok(data) = form.validate() {
                        let d = data.clone();
                        match <$form as $trait>::on_post(&mut form, d, &$req).await {
                            Ok($post) => {
                                let r: Result<anansi::web::Response> = $b;
                                if r.is_ok() {
                                    return r;
                                }
                            },
                            Err(error) => {
                                if let Ok(fe) = error.downcast::<anansi::forms::SimpleError>() {
                                    form.add_error(fe);
                                }
                            },
                        }
                        form.set_data(Some(data));
                    }
                    async {
                        form.fill()?;
                        form.post(&$req.token()?);
                        Ok(form)
                    }.await
                } else {
                    res
                }
            }
        }
    }
}

#[macro_export]
macro_rules! _handle_or_404 {
    ($form:ty, $trait:ty, $req:ident, $post:ident, $b:expr) => {
        match *$req.method() {
            anansi::web::GET => {
                use anansi::forms::Form;
                let mut form = match <$form as $trait>::on_get(&$req).await {
                    Ok(form) => form,
                    Err(_) => return Err(anansi::db::invalid()),
                };
                form.post(&$req.token()?);
                Ok(form)
            },
            anansi::web::POST => {
                use anansi::forms::Form;
                let res = <$form>::from_post(&mut $req);
                if let Ok(mut form) = res {
                    if let Ok(data) = form.validate() {
                        let d = data.clone();
                        let r0 = <$form as $trait>::on_post(&mut form, d, &$req).await;
                        if let Ok($post) = r0 {
                            let r: Result<anansi::web::Response> = $b;
                            if r.is_ok() {
                                return r;
                            }
                        } else {
                            return Err(Box::new(anansi::web::Http404::from($req)));
                        }
                        form.set_data(Some(data));
                    }
                    async {
                        form.fill()?;
                        form.post(&$req.token()?);
                        Ok(form)
                    }.await
                } else {
                    res
                }
            },
        }
    }
}

#[macro_export]
macro_rules! base_handle {
    ($form:ty, $trait:ty, $req:ident, $post:ident, $b:expr, $on_get:block, $on_post:block) => {
        {
            match *$req.method() {
                anansi::web::GET => {
                    async {
                        $on_get
                    }.await
                },
                anansi::web::POST => {
                    $on_post
                },
            }
        }
    }
}

#[macro_export]
macro_rules! form_error {
    ($msg:literal) => {
        Err(Box::new(anansi::forms::FormError::new($msg)))
    }
}

pub trait HasModel {
    type Item: Model + Send + Sync;
}

#[async_trait]
pub trait Form {
    type Data: Clone + Send;
    fn new() -> Self where Self: Sized; 
    fn attrs(&self) -> &Attributes;
    fn csrf_token(&self) -> Option<&String>;
    fn insert_attr(self, key: &str, value: &str) -> Self where Self: Sized;
    fn post(&mut self, token: &TokenRef);
    async fn from_data(data: Self::Data) -> Self where Self: Sized;
    fn from_post<B: BaseRequest + CsrfDefense>(req: &mut B) -> Result<Self> where Self: Sized;
    fn fill(&mut self) -> Result<()> where Self: Sized;
    fn validate(&mut self) -> Result<Self::Data>;
    fn errors(&self) -> &FormErrors;
    fn add_error(&mut self, e: Box<dyn Error + Send + Sync>);
    fn set_data(&mut self, data: Option<Self::Data>);
    fn field_names() -> &'static [&'static str];
    fn field(&self, n: usize) -> Option<&dyn Field>;
    fn token_tag(&self) -> Option<String> {
        if let Some(csrf_token) = self.csrf_token() {
            Some(format!("<input type=\"hidden\" name=\"csrf_token\" value=\"{}\">", csrf_token))
        } else {
            None
        }
    }
    fn tag(&self) -> String {
        let mut tag = String::from("<form");
        for (name, value) in &self.attrs().attrs {
            tag.push_str(&format!(" {}=\"{}\"", name, value));
        }
        if self.csrf_token().is_some() {
            tag.push_str(" method=\"post\">");
        } else {
            tag.push('>');
        }
        tag
    }
    fn action<B: BaseRequest + Reverse>(self, req: &B, action: View<B>) -> Self where Self: Sized {
        self.insert_attr("action", &anansi::url!(req, action))
    }
    fn class(self, class: &str) -> Self where Self: Sized {
        self.insert_attr("class", class)
    }
    fn id(self, id: &str) -> Self where Self: Sized {
        self.insert_attr("id", id)
    }
    fn submit(&self, value: &str) -> String {
        format!("<input type=\"submit\" value=\"{value}\">")
    }
}

#[async_trait]
pub trait GetData<B: BaseRequest + ParamsToModel>: Form + HasModel where <Self as HasModel>::Item: FromParams {
    fn from_map(form_map: FormMap) -> Result<<Self as Form>::Data>;
    async fn from_model(model: <Self as HasModel>::Item, req: &B) -> Result<<Self as Form>::Data>;
    async fn get_data(req: &B) -> Result<<Self as Form>::Data> {
        if let Ok(form_map) = req.to_form_map() {
            Self::from_map(form_map)
        } else {
            Self::from_model(req.to_model().await?, req).await
        }
    }
}

#[async_trait]
pub trait ToEdit<B: BaseRequest + ParamsToModel>: Form + GetData<B> where <Self as HasModel>::Item: FromParams {
    async fn on_get(req: &B) -> Result<Self> where <Self as Form>::Data: Send, Self: Sized {
        let mut form = Self::from_data(Self::get_data(req).await?).await;
        form.fill()?;
        Ok(form)
    }

    async fn on_post(&mut self, data: <Self as Form>::Data, req: &B) -> Result<Self::Item>;
}

#[derive(Clone)]
pub struct Attributes {
    attrs: HashMap<String, String>,
}

impl Attributes {
    pub fn new() -> Self {
        Self {attrs: HashMap::new()}
    }
    pub fn get(&self, key: &str) -> Result<&String> {
        self.attrs.get(key).ok_or(invalid())
    }
    pub fn insert(&mut self, key: &str, value: &str) {
        self.attrs.insert(key.to_string(), value.to_string());
    }
    pub fn pass(mut self, key: &str, value: &str) -> Self {
        self.insert(key, value);
        self
    }
    pub fn id(mut self, id: &'static str) -> Self {
        self.insert("id", id);
        self
    }
}

impl fmt::Display for Attributes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, value) in &self.attrs {
            match name.as_str() {
                "required" => write!(f, " {}", name)?,
                _ => write!(f, " {}=\"{}\"", name, value)?,
            }
        }
        write!(f, ">")
    }
}

pub type BoxWidget = Box<dyn Widget + Send>;

pub struct FormField {
    pub label: &'static str,
    pub widget: BoxWidget,
    pub errors: FormErrors,
}

macro_rules! field {
    ($id:ident) => {
        pub struct $id(FormField);
        impl $id {
            pub fn new(label: &'static str, widget: BoxWidget) -> Self {
                Self {0: FormField::new(label, widget)}
            }
        }
        impl Field for $id {
            fn label(&self) -> &'static str {
                self.0.label
            }
            fn widget(&self) -> &BoxWidget {
                &self.0.widget
            }
            fn mut_widget(&mut self) -> &mut BoxWidget {
                &mut self.0.widget
            }
            fn errors(&self) -> &FormErrors {
                &self.0.errors
            }
            fn add_error(&mut self, e: Box<dyn Error + Send + Sync>) {
                self.0.errors.add_error(e);
            }
        }
    }
}

macro_rules! widget {
    ($id:ident) => {
        impl Widget for $id {
            fn name(&self) -> &'static str {
                self.name
            }
            fn attrs(&self) -> &Attributes {
                &self.attrs
            }
            fn mut_attrs(&mut self) -> &mut Attributes {
                &mut self.attrs
            }
        }
    }
}

pub struct VarChar<const N: u16>(FormField);

impl FormField {
    pub fn new(label: &'static str, widget: BoxWidget) -> Self {
        Self {label, widget, errors: FormErrors::from("field-errors")}
    }
}

impl<const N: u16> VarChar<N> {
    pub fn new(label: &'static str, widget: BoxWidget) -> Self {
        Self {0: FormField::new(label, widget)}
    }
}

field!(MultipleChoice);

field!(Boolean);

pub trait Widget: fmt::Display {
    fn name(&self) -> &'static str;
    fn attrs(&self) -> &Attributes;
    fn mut_attrs(&mut self) -> &mut Attributes;
}

#[derive(Clone)]
pub struct Checkbox {
    pub name: &'static str,
    pub attrs: Attributes,
}

widget!(Checkbox);

#[derive(Clone)]
pub struct Text {
    pub name: &'static str,
    pub attrs: Attributes,
}

widget!(Text);

#[derive(Clone)]
pub struct Password {
    pub name: &'static str,
    pub attrs: Attributes,
}

widget!(Password);

#[derive(Clone)]
pub struct SelectMultiple {
    pub name: &'static str,
    pub attrs: Attributes,
    options: Vec<String>,
}

widget!(SelectMultiple);

impl<const N: u16> Field for VarChar<N> {
    fn label(&self) -> &'static str {
        self.0.label
    }
    fn widget(&self) -> &BoxWidget {
        &self.0.widget
    }
    fn mut_widget(&mut self) -> &mut BoxWidget {
        &mut self.0.widget
    }
    fn errors(&self) -> &FormErrors {
        &self.0.errors
    }
    fn add_error(&mut self, e: Box<dyn Error + Send + Sync>) {
        self.0.errors.add_error(e);
    }
}

impl<const N: u16> fmt::Display for VarChar<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,  "{}", self.widget())
    }
}

impl fmt::Display for MultipleChoice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,  "{}", self.widget())
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,  "{}", self.widget())
    }
}

pub trait Field: fmt::Display {
    fn label(&self) -> &'static str;
    fn widget(&self) -> &BoxWidget;
    fn mut_widget(&mut self) -> &mut BoxWidget;
    fn errors(&self) -> &FormErrors;
    fn add_error(&mut self, err: Box<dyn Error + Send + Sync>);
    fn label_tag(&self) -> String {
        format!("<label for=\"{}\">{}</label>", self.widget().name(), self.label())
    }
}

#[async_trait]
pub trait ToEmpty<B: BaseRequest>: Form {
    async fn on_get(_req: &B) -> Result<Self> where Self: Sized {
        Ok(Self::new())
    }

    async fn on_post(&mut self, _data: <Self as Form>::Data, _req: &B) -> Result<()> where <Self as Form>::Data: Send + Sync {
        Ok(())
    }
}

#[async_trait]
pub trait ToModel<B: BaseRequest>: Form + HasModel {
    async fn on_get(_req: &B) -> Result<Self> where Self: Sized {
        Ok(Self::new())
    }

    async fn on_post(&mut self, data: <Self as Form>::Data, req: &B) -> Result<Self::Item>;
}

impl fmt::Display for Checkbox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<input type=\"checkbox\" name=\"{}\"{}", self.name, self.attrs)
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<input type=\"text\" name=\"{}\"{}", self.name, self.attrs)
    }
}

impl fmt::Display for Password {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<input type=\"password\" name=\"{}\"{}", self.name, self.attrs)
    }
}

impl fmt::Display for SelectMultiple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for option in &self.options {
            s.push_str(&format!("<option value=\"{}\">{0}</option>\n", option));
        }
        write!(f, "<select name=\"{}\"{} multiple>\n{}", self.name, self.attrs, s)
    }
}

pub struct FormErrors {
    class: &'static str,
    errors: Vec<Box<dyn Error + Send + Sync>>,
}

impl FormErrors {
    pub fn new() -> Self {
        Self::from("form-errors")
    }
    pub fn from(class: &'static str) -> Self {
        Self {class, errors: Vec::new()}
    }
    pub fn add_error(&mut self, error: Box<dyn Error + Send + Sync>) {
        self.errors.push(error);
    }
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}

impl fmt::Display for FormErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = String::new();
        for error in &self.errors {
            list.push_str(&format!("<li>{}</li>", error));
        }
        write!(f, "<ul class=\"{}\">{}</ul>", self.class, list)
    }
}

#[derive(Debug)]
pub struct SimpleError {
    msg: String,
}

impl SimpleError {
    pub fn from_str(m: &str) -> Self {
        Self {msg: m.to_string()}
    }
    pub fn from(msg: String) -> Self {
        Self {msg}
    }
}

impl fmt::Display for SimpleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for SimpleError {}

#[derive(Debug)]
pub struct FormError {
    kind: FormErrorKind,
}

impl FormError {
    pub fn new(msg: &str) -> SimpleError {
        SimpleError::from(msg.to_string())
    }
    pub fn from(kind: FormErrorKind) -> Self {
        Self {kind}
    }
    pub fn kind(&self) -> &FormErrorKind {
        &self.kind
    }
}

impl fmt::Display for FormError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Error for FormError {}

#[derive(Debug)]
#[non_exhaustive]
pub enum FormErrorKind {
    Required,
}

impl fmt::Display for FormErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Required => "This field is required.",
        };
        write!(f, "{}", s)
    }
}
