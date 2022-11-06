pub mod views;
pub mod site;

use anansi::{path_literal, statics};

pub const APP_NAME: &'static str = "admin";

statics! {
    path_literal!("admin", "style.css"),
    path_literal!("admin", "login.css"),
}
