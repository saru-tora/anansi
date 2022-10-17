extern crate self as anansi;

pub use anansi_macros::*;

pub mod server;
pub mod web;
pub mod db;
pub mod models;
pub mod humanize;
mod datetime;
pub mod router;
pub mod forms;
pub mod migrations;
pub mod admin_site;

#[cfg(test)]
mod tests {
    #[test]
    fn migrate() {
        let current = std::env::current_dir().unwrap();
        let mut sql = String::from("anansi::operations! {\n");
        let mut v = vec![];
        let app = "sessions";
        let s = format!("{}/../anansi/src/util/{}/models.rs", current.into_os_string().into_string().unwrap(), app);
        let content = std::fs::read_to_string(s).unwrap();
        anansi::migrations::process_syntax(app, content, &mut v);
        anansi::migrations::new_syntax(&mut sql, v);
        println!("{}}}", sql);
    }
}
