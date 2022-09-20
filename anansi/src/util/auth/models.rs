use std::{fmt, result};

use pbkdf2::{
    password_hash::{
        rand_core::OsRng,
        PasswordHash, PasswordHasher, PasswordVerifier, SaltString
    },
    Pbkdf2
};

use anansi::web::{Result};
use anansi::db::{DbPool, invalid};
use anansi::models::{Model, BigInt, Boolean, DataType, VarChar, ManyToMany};
use anansi::model;

#[model]
#[derive(Debug, Clone)]
pub struct User {
    #[field(unique = "true")]
    pub username: VarChar<150>,
    pub password: VarChar<150>,
    pub groups: ManyToMany<Group>,
    pub is_admin: Boolean,
}

pub fn hash_password(password: &str) -> Result<VarChar<150>> {
    let salt = SaltString::generate(&mut OsRng);
    let password_hash = match Pbkdf2.hash_password(password.as_bytes(), &salt) {
        Ok(o) => o,
        Err(_) => return Err(invalid()),
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
        "Valid usernames cannot contain control characters."
    }
    pub fn into_username(self) -> Option<VarChar<150>> {
        self.username
    }
}

impl User {
    pub const KEY: &'static str = "_user_id";
    pub fn is_admin(&self) -> bool {
        self.is_admin == true
    }
    pub fn is_auth(&self) -> bool {
        self.id != BigInt::new(0)
    }
    pub async fn validate_username(username: &str, pool: &DbPool) -> result::Result<VarChar<150>, UsernameFeedback> {
        let username = username.trim();
        if username.is_empty() || username.contains(char::is_control) {
            return Err(UsernameFeedback::from(username.to_string()));
        }
        if let Ok(username) = VarChar::from(username.to_string()) {
            if let Ok(n) = Self::count().whose(user::username().eq().data_ref(&username)).raw_get(pool).await {
                if n == 0 {
                    return Ok(username);
                }
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
        Self {
            id: BigInt::new(0),
            username: VarChar::from("guest".to_string()).unwrap(),
            password: VarChar::new(),
            groups: ManyToMany::new(),
            is_admin: Boolean::new(false),
        }
    }
    pub fn verify(&self, password: &VarChar<150>) -> Result<()> {
        let parsed_hash = match PasswordHash::new(&self.password) {
            Ok(p) => p,
            Err(_) => return Err(invalid()),
        };
        if Pbkdf2.verify_password(&password.as_bytes(), &parsed_hash).is_ok() { 
            Ok(())
        } else {
            Err(invalid())
        }
    }
}

#[model]
#[derive(Debug, Clone)]
pub struct Group {
    pub name: VarChar<150>,
}
