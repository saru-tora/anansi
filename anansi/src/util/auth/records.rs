use std::{fmt, result};

use pbkdf2::{
    password_hash::{
        rand_core::OsRng,
        PasswordHash, PasswordHasher, PasswordVerifier, SaltString
    },
    Pbkdf2
};

use async_recursion::async_recursion;

use totp_rs::{Algorithm, TOTP, Secret};

use anansi::web::{Result, BaseUser, BaseRequest, WebErrorKind};
use anansi::db::{DbPool, DbRowVec};
use anansi::records::{Record, BigInt, VarChar, Text, DateTime, DataType};
use anansi::{record, FromParams, ToUrl, Relate};

#[record]
#[derive(Debug, Clone, FromParams, ToUrl, Relate)]
pub struct User {
    #[field(unique = "true")]
    pub username: VarChar<150>,
    pub email: Option<Text>,
    pub password: VarChar<150>,
    pub secret: Option<Text>,
    #[field(auto_now_add = "true")]
    pub last_login: DateTime,
    #[field(auto_now_add = "true")]
    pub date_joined: DateTime,
}

impl BaseUser for User {
    type Name = VarChar<150>;
    type Secret = Text;
    fn username(&self) -> &Self::Name {
        &self.username
    }
    fn secret(&self) -> &Option<Self::Secret> {
        &self.secret
    }
    fn is_auth(&self) -> bool {
        self.id != BigInt::new(0)
    }
}

pub fn hash_password(password: &str) -> Result<VarChar<150>> {
    let salt = SaltString::generate(&mut OsRng);
    let password_hash = match Pbkdf2.hash_password(password.as_bytes(), &salt) {
        Ok(o) => o,
        Err(_) => return Err(WebErrorKind::BadPassword.to_box()),
    };
    VarChar::from_val(password_hash.to_string())
}

pub struct Entropy(f64);

pub enum Warning {
    Weak,
    Medium,
}

pub struct Feedback {
    warning: Warning,
    suggestion: &'static str,
}

impl Feedback {
    fn new(warning: Warning) -> Self {
        Self {warning, suggestion: "Consider using a password manager."}
    }
    pub fn warning(&self) -> &Warning {
        &self.warning
    }
    pub fn suggestion(&self) -> &'static str {
        self.suggestion
    }
}

impl fmt::Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Weak => "weak",
            Self::Medium => "medium",
        };
        write!(f, "Strength: {}.", s)
    }
}

impl Entropy {
    pub fn feedback(&self) -> Option<Feedback> {
        if self.0 >= 77.5 {
            None
        } else {
            let w = if self.0 > 45.6 {
                Warning::Medium
            } else {
                Warning::Weak
            };
            Some(Feedback::new(w))
        }
    }
}

pub struct UsernameFeedback {
    username: Option<VarChar<150>>,
}

impl UsernameFeedback {
    pub fn new() -> Self {
        Self {username: None}
    }
    pub fn from(s: String) -> Self {
        let username = match VarChar::from(s) {
            Ok(v) => Some(v),
            Err(_) => None,
        };
        Self {username}
    }
    pub fn warning(&self) -> &'static str {
        "This username is invalid or already taken."
    }
    pub fn suggestion(&self) -> &'static str {
        "Valid usernames can only contain letters, numbers, dashes, and underscores."
    }
    pub fn into_username(self) -> Option<VarChar<150>> {
        self.username
    }
}

pub struct BaseRelation {
    pub subject_namespace: String,
    pub subject_key: i64,
    pub subject_predicate: Option<String>,
}

impl BaseRelation {
    pub fn from<V: DbRowVec>(rows: V) -> Result<Vec<Self>> {
        let mut v = vec![];
        for row in rows {
            use anansi::db::DbRow;
            let subject_namespace = row.try_string("subject_namespace")?;
            let subject_key = row.try_i64("subject_key")?;
            let subject_predicate = row.try_option_string("subject_predicate")?;
            v.push(Self {subject_namespace, subject_key, subject_predicate})
        }
        Ok(v)
    }
    pub fn search(object_namespace: &str, object_key: i64, object_predicate: &str) -> String {
        use anansi::records::ToSql;
        let q = format!("SELECT * FROM \"{}\" WHERE object_key = {} AND object_predicate = {};", format!("{}tuple", object_namespace), object_key, object_predicate.to_sql());
        q
    }
    #[async_recursion]
    pub async fn check<B: BaseRequest>(object_namespace: &str, object_key: i64, object_predicate: &str, req: &B) -> anansi::web::Result<()> {
        let rels = Self::from(req.raw().pool().query(&Self::search(object_namespace, object_key, object_predicate)).await?)?;
        for rel in rels {
            match rel.subject_predicate {
                None => {
                    if rel.subject_namespace == "auth_user" && rel.subject_key == req.user().pk().as_i64() {
                        return Ok(());
                    }
                },
                Some(predicate) => {
                    if Self::check(&rel.subject_namespace, rel.subject_key, &predicate, req).await.is_ok() {
                        return Ok(());
                    }
                },
            }
        }
        Err(WebErrorKind::BadRelation.to_box() as Box<dyn std::error::Error + Send + Sync + 'static>)
    }
}

impl User {
    pub const KEY: &'static str = "_user_id";
    pub const TOTP_KEY: &'static str = "_temp_totp";
    
    pub async fn validate_username<D: DbPool>(username: &str, pool: &D) -> result::Result<VarChar<150>, UsernameFeedback> {
        if username.is_empty() {
            return Err(UsernameFeedback::from(username.to_string()));
        }
        for c in username.chars() {
            if !(c.is_ascii_alphanumeric() || c == '_' || c == '-') {
                return Err(UsernameFeedback::from(username.to_string()));
            }
        }
        if let Ok(username) = VarChar::from(username.to_string()) {
            match Self::count().whose(user::username().eq(&username)).raw_get(pool).await {
                Ok(n) => if n == 0 {
                    return Ok(username);
                }
                Err(_) => {}
            }
        }
        Err(UsernameFeedback::from(username.to_string()))
    }
    pub fn check_password(password: &str) -> Entropy {
        let mut n: usize = 0;
        if password.contains(char::is_uppercase) {
            n += 26;
        }
        if password.contains(char::is_lowercase) {
            n += 26;
        }
        if password.contains(char::is_numeric) {
            n += 10;
        }
        for c in password.chars() {
            if c.is_ascii_punctuation() {
                n += 32;
                break;
            }
        }
        if password.contains(char::is_whitespace) {
            n += 1;
        }
        let entropy = (n as f64).powf(password.len() as f64).log(2.0);
        Entropy {0: entropy}
    }
    pub fn guest() -> Self {
        let now = DateTime::now();
        Self {
            id: BigInt::new(0),
            username: VarChar::from("guest".to_string()).unwrap(),
            email: None,
            password: VarChar::new(),
            secret: None,
            last_login: now,
            date_joined: now,
        }
    }
    pub fn verify(&self, password: &VarChar<150>) -> Result<()> {
        let parsed_hash = match PasswordHash::new(&self.password) {
            Ok(p) => p,
            Err(_) => return Err(WebErrorKind::BadPassword.to_box()),
        };
        if Pbkdf2.verify_password(&password.as_bytes(), &parsed_hash).is_ok() { 
            Ok(())
        } else {
            Err(WebErrorKind::BadPassword.to_box())
        }
    }
    pub fn set_totp(&mut self, issuer: Option<String>, secret: String) -> Result<()> {
        self.check_totp(issuer, &secret)?;
        self.secret = Some(Text::from(secret));
        Ok(())
    }
    pub fn check_totp(&self, issuer: Option<String>, secret: &str) -> Result<()> {
        if let Some(s) = &self.secret {
            let totp = TOTP::new(
                Algorithm::SHA1,
                6,
                1,
                30,
                Secret::Raw(s.to_string().into_bytes().to_vec()).to_bytes().unwrap(),
                issuer,
                self.username().to_string(),
            )?;
            if secret == totp.generate_current()? {
                return Ok(());
            }
        }
        Err(WebErrorKind::BadTotp.to_box())
    }
}

#[record]
#[derive(Debug, Clone, Relate, FromParams, ToUrl)]
pub struct Group {
    pub groupname: VarChar<150>,
}

impl Group {
    pub async fn is_admin<B: BaseRequest>(req: &B) -> Result<()> {
        BaseRelation::check("auth_group", 1, "member", req).await
    }
}

#[record]
#[derive(Relate, FromParams, ToUrl)]
pub struct Filter {
    pub table_name: Text,
    pub filter_name: Text,
    pub filter: Text,
    pub raw_query: Text,
}
