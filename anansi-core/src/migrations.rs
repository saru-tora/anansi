use std::{str, fs, fmt};
use std::thread::LocalKey;
use std::collections::HashMap;
use quote::quote;
use syn::Type;
use syn::Field;
use syn::Item::Struct;
use syn::Fields::Named;
use syn::Attribute;
use sqlx::Row;
use crate::db::{DbPool, unescape};
use crate::models::ModelField;

#[macro_export]
macro_rules! apps {
    ($($name:ident,)*) => {
        static APP_MIGRATIONS: &[std::thread::LocalKey<anansi::migrations::AppMigration>] = &[
            $($name::migrations::init::MIGRATIONS,)*
        ];
    }
}

#[macro_export]
macro_rules! local_migrations {
    ($($name:literal,)*) => {
        thread_local!(pub static MIGRATIONS: anansi::migrations::AppMigration = (
            super::super::init::APP_NAME, vec![
            $(($name, include!($name)),)*
        ]));
    }
}

#[macro_export]
macro_rules! operations {
    ($($e:expr,)*) => {
        vec![
            $(Box::new($e),)*
        ]
    }
}

pub type AppMigration = (&'static str, Vec<Migration>);
pub type Migration = (&'static str, Vec<Box<dyn fmt::Display>>);

pub mod prelude {
    pub use anansi::{models, migrations, local_migrations};
}

#[derive(Clone)]
pub struct RunSql {
    s: &'static str,
}

impl RunSql {
    pub fn new(s: &'static str) -> Self {
        Self {s}
    }
}

impl fmt::Display for RunSql {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n\n", self.s)
    }
}

#[derive(Clone)]
pub struct CreateModel {
    pub prefix: &'static str,
    pub name: &'static str,
    pub fields: Vec<(&'static str, ModelField)>,
}

impl fmt::Display for CreateModel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = format!("CREATE TABLE \"{}_{}\" (", self.prefix, self.name);
        let mut v = vec![];
        for (field_name, field) in &self.fields {
            let (syn, con) = field.to_syntax();
            s.push_str(&format!("\n\t\"{}\" {},", field_name, syn));
            if !con.is_empty() {
                v.push(con);
            }
        }
        for con in v {
            for c in con {
                s.push_str(&format!("\n\t{}", c));
            }
            s.push(',');
        }
        s.pop().unwrap();
        write!(f, "{}\n);\n\n", s)
    }
}

impl IsMigration for CreateModel {}

pub trait IsMigration: fmt::Display {}

fn to_migration(v: &Vec<Box<dyn fmt::Display>>) -> String {
    let mut s = "BEGIN;\n\n".to_string();
    for d in v {
        s.push_str(&d.to_string());
    }
    s.push_str("COMMIT;");
    s
}

pub async fn migrate(app_migrations: &'static [LocalKey<AppMigration>], pool: &DbPool) {
    for app_migration in app_migrations {
        let mut app = "";
        let mut migrations = vec![];
        app_migration.with(|am| {
            app = am.0;
            for migration in &am.1 {
                migrations.push((migration.0, to_migration(&migration.1)));
            }
        });
        println!("Checking {}", app);
        let rows = sqlx::query("SELECT name FROM anansi_migrations WHERE app = ?").bind(app).fetch_all(&pool.0).await.unwrap();
        let mut names = vec![];
        for row in rows {
            let name: String = row.try_get("name").unwrap();
            names.push(name);
        }
        for (n, q) in migrations {
            if !names.contains(&n.to_string()) {
                println!("    Applying migration \"{}\"", n);
                sqlx::query(&q).execute(&pool.0).await.unwrap();
                sqlx::query("INSERT INTO anansi_migrations (app, name, applied) VALUES(?, ?, strftime('%Y-%m-%d %H-%M-%f','now'))").bind(app).bind(n).execute(&pool.0).await.unwrap();
            }
        }
    }
}

pub async fn sql_migrate(app_migrations: &'static [LocalKey<AppMigration>], app_name: &str, migration_name: &str) {
    for app_migration in app_migrations {
        app_migration.with(|am| {
            if am.0 == app_name {
                for migration in &am.1 {
                    if migration.0 == migration_name {
                        println!("{}", to_migration(&migration.1));
                        return;
                    }
                }
            }
        });
    }
}

pub async fn make_migrations(app_dir: &str, pool: &DbPool) {
    let app_dir = if app_dir.chars().last().unwrap() == '/' {
        app_dir.to_string()
    } else {
        format!("{}/", app_dir)
    };
    let mut v = vec![];
    let s: Vec<&str> = app_dir.split('/').collect();
    let app_name = &s[s.len()-2];
    let mfile = format!("{}models.rs", app_dir);
    let content = fs::read_to_string(&mfile).expect(&format!("could not open {}", mfile));
    process_syntax(app_name, content, &mut v);

    let mut syntaxes = Vec::new();
    let mut new_models = Vec::new();
    for (prefix, name, syntax) in v {
        let val = String::from(format!("SELECT schema FROM models WHERE name = '{}';\n", name));
        if let Ok(row) = sqlx::query(&val).fetch_one(&pool.0).await {
            let s: &str = row.try_get("schema").unwrap();
            if unescape(&s) == syntax {
                continue;
            } else {
                syntaxes.push((prefix, name, syntax));
            }
        } else {
            new_models.push((prefix, name, syntax));
        }
    }
    if !(new_models.is_empty() && syntaxes.is_empty()) {
        let mut sql = String::from("anansi::operations! {\n");
        new_syntax(&mut sql, new_models);
        add_syntax(&mut sql, syntaxes);
        
        sql.push_str("}");
        let row = sqlx::query("SELECT COUNT(*) as count FROM anansi_migrations WHERE app = ?").bind(app_name).fetch_one(&pool.0).await.unwrap();
        let n: u16 = row.try_get("count").unwrap();
        let mname = format!("{:04}", n+1);
        let mdir = format!("{}migrations/", app_dir);
        let s = format!("{}{}", mdir, mname);
        fs::write(&s, sql).unwrap();
        println!("Created \"{}\"", s);
        let idir = format!("{}init.rs", mdir);
        let original = fs::read_to_string(&idir).unwrap();
        if original.trim() == "use anansi::migrations::prelude::*;\n\nlocal_migrations! {}" {
            fs::write(idir, format!("use anansi::migrations::prelude::*;\n\nlocal_migrations! {{\n    \"{}\",\n}}", mname)).unwrap();
        } else {
            let split = original.rsplit_once('}').unwrap();
            fs::write(idir, format!("{}    \"{}\",\n}}", split.0, mname)).unwrap();
        }
    }
}

pub fn new_syntax(sql: &mut String, new_models: Vec<(String, String, String)>) {
    for (prefix, name, syntax) in new_models {
            sql.push_str(&format!("    migrations::CreateModel {{\n        prefix: \"{}\",\n        name: \"{}\",\n        fields: vec![\n", prefix, name));
            sql.push_str(&syntax);
            sql.push_str("        ],\n    },\n");
        }
}

pub fn add_syntax(sql: &mut String, syntaxes: Vec<(String, String, String)>) {
    for (prefix, name, syntax) in syntaxes {
        let temp_prefix = format!("_{}", prefix);
        sql.push_str(&format!("    migrations::CreateModel {{\n        prefix: \"{}\",\n        name: \"{}\",\n        fields: vec![\n", temp_prefix, name));
        sql.push_str(&syntax);
        sql.push_str("        ],\n    },\n");
        let temp_name = format!("{}_{}", temp_prefix, name);
        let full_name = format!("{}_{}", prefix, name);
        sql.push_str(&format!("    migrations::RunSql::new(\"INSERT INTO {} SELECT * FROM {};\"),\n", temp_name, full_name));
        sql.push_str(&format!("    migrations::RunSql::new(\"DROP TABLE {};\"),\n", full_name));
        sql.push_str(&format!("    migrations::RunSql::new(\"ALTER TABLE {} RENAME TO {};\"),\n", temp_name, full_name));
    }
}

fn get_attrs(attrs: &Vec<Attribute>) -> HashMap<String, String> {
    let mut hm = HashMap::new();
    for attr in attrs {
        if attr.path.segments[0].ident.to_owned() == "field" {
            let tokens = &attr.tokens;
            let tokens = quote! {#tokens}.to_string();
            let args = tokens[1..tokens.len()-1].split(',');
            for arg in args {
                let (key, value) = arg.split_once('=').unwrap();
                hm.insert(key.trim().to_owned(), value.trim().to_owned());
            }
            break;
        }
    }
    hm
}

pub fn process_syntax(db: &str, content: String, v: &mut Vec<(String, String, String)>) {
    let alter = false;
    let syntax = syn::parse_file(&content).expect("Unable to parse file");
    for item in syntax.items {
        match item {
            Struct(item) => {
                let mut sql = String::new();
                let name = item.ident.to_string().to_lowercase();
                let prefix = if !alter {
                    format!("{}", db)
                } else {
                    format!("_{}", db)
                };
                let mut is_model = false;
                for attr in item.attrs {
                    if attr.path.segments.last().unwrap().ident.to_string() == "model" {
                        is_model = true;
                        break;
                    }
                }
                if !is_model {
                    continue;
                }
                match item.fields {
                    Named(named) => {
                        let mut meta = Vec::new();
                        for field in named.named {
                            let fieldname = field.ident.as_ref().unwrap().to_string();
                            if let Some(ty) = get_type(&fieldname, &field, &mut meta, db) {
                                sql.push_str(&format!("            (\n                \"{}\",\n                models::{}\n            ),\n", fieldname, ty));
                            }
                        }
                        if !sql.contains("PRIMARY KEY") {
                            sql = "            (\n                \"id\",\n                models::BigInt::field().primary_key()\n            ),\n".to_string() + &sql;
                        }
                        v.push((prefix.to_string(), name.to_string(), sql));
                        if !alter {
                            key_table(meta, v, &prefix, &name);
                        }
                    },
                    _ => unimplemented!(),
                }
            },
            _ => {},
        }
    }
}

fn get_type(fieldname: &String, field: &Field, meta: &mut Vec<Vec<String>>, db: &str) -> Option<String> {
    let ty = &field.ty;
    Some(
        match ty {
            Type::Path(path) => {
                let segment = path.path.segments[0].ident.to_string();
                let attrs = get_attrs(&field.attrs);
                let mut ty = match segment.as_str() {
                    "BigInt" => {
                        let mut s = format!("BigInt::field()");
                        if let Some(pk) = attrs.get("primary_key") {
                            if pk == "true" {
                                s.push_str(".primary_key()");
                            }
                        }
                        s
                    },
                    "ManyToMany" => {
                        let mut v = Vec::new();
                        v.push(fieldname.clone());
                        v.push(segment.clone());
                        let s = parse_type(ty);
                        v.push(s);
                        meta.push(v);
                        return None;
                    },
                    "ForeignKey" => {
                        let mut s = String::new();
                        s.push_str("BigInt::field()");
                        let m = parse_type(ty);
                        let m: Vec<&str> = m.split(',').collect();
                        let m = m[0].trim().to_lowercase();
                        let parent: Vec<&str> = m.split("::").collect();
                        let parent_app = if parent.len() > 2 {
                            parent[parent.len() - 3].trim().to_string()
                        } else {
                            db.to_string()
                        };
                        let parent_name = parent.last().unwrap().trim().to_string();
                        s.push_str(&format!(".foreign_key(\"{}\", \"{}\", \"id\")", parent_app, parent_name));
                        s
                    },
                    "DateTime" => {
                        format!("DateTime::field()")
                    },
                    "Boolean" => {
                        format!("Boolean::field()")
                    },
                    "VarChar" => {
                        let n: u16 = parse_type(ty).parse().unwrap();
                        format!("VarChar::<{}>::field()", n)
                    },
                    "Text" => {
                        format!("Text::field()")
                    },
                    _ => {
                        unimplemented!()
                    },
                };
                if let Some(_) = attrs.get("unique") {
                    ty.push_str(".unique()");
                }
                ty
            },
            _ => unimplemented!(),
        }
    )
}

fn parse_type(ty: &Type) -> String {
    let q = quote! {#ty}.to_string();
    let (_, s) = q.split_once('<').unwrap();
    let (s, _) = s.split_once('>').unwrap();
    s.trim().to_string()
}

fn key_table(m2: Vec<Vec<String>>, v: &mut Vec<(String, String, String)>, prefix: &str, name: &str) {
    for m in m2 {
        match m[1].as_str() {
            "ManyToMany" => {
                let other = m[2].to_lowercase();
                let mut sql = String::new();
                sql.push_str(&format!("            (\n                \"{}\",\n                models::BigInt::field()", name));
                sql.push_str(&format!(".foreign_key(\"{}\", \"{}\", \"id\")\n            ),\n", prefix, name));
                sql.push_str(&format!("            (\n                \"{}\",\n                models::BigInt::field()", other));
                sql.push_str(&format!(".foreign_key(\"{}\", \"{}\", \"id\")\n            ),\n", prefix, other));
                v.push((prefix.to_string(), format!("{}_{}", name, other), sql));
            },
            _ => panic!("error populating table"),
        }
    }
}
