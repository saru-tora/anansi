use std::{str, fs, path::PathBuf};
use std::collections::HashMap;
use quote::quote;
use syn::Type;
use syn::Field;
use syn::Item::Struct;
use syn::Fields::Named;
use syn::Attribute;
use anansi::raw_transact;
use crate::db::{DbPool, DbType, unescape, DbRow};
use crate::records::RecordField;

#[macro_export]
macro_rules! apps {
    ($($name:ident,)*) => {
        fn app_migrations<D: anansi::db::DbPool>() -> Vec<anansi::migrations::AppMigration<D>> {
            vec![$($name::migrations::migrations(),)*]
        }
    }
}

#[macro_export]
macro_rules! local_migrations {
    ($($name:literal,)*) => {
        pub fn migrations<D: anansi::db::DbPool>() -> anansi::migrations::AppMigration<D> {
            (
                super::APP_NAME.to_string(),
                vec![$(($name.to_string(), include!($name)),)*]
            )
        }
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

pub type AppMigration<D> = (String, Vec<Migration<D>>);
pub type Migration<D> = (String, Vec<Box<dyn ToQuery<D>>>);

pub mod prelude {
    pub use anansi::{records, migrations, local_migrations};
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

impl<D: DbType> ToQuery<D> for RunSql {
    fn to_query(&self) -> String {
        format!("{}\n\n", self.s)
    }
}

#[derive(Clone)]
pub struct CreateRecord {
    pub prefix: &'static str,
    pub name: &'static str,
    pub fields: Vec<(&'static str, RecordField)>,
}

impl<D: DbType> ToQuery<D> for CreateRecord {
    fn to_query(&self) -> String {
        let mut s = format!("CREATE TABLE \"{}_{}\" (", self.prefix, self.name);
        let mut v = vec![];
        for (field_name, field) in &self.fields {
            let (syn, con, index) = field.to_syntax::<D>();
            s.push_str(&format!("\n\t\"{}\" {}", field_name, syn));
            if !con.is_empty() {
                for c in con {
                    s.push_str(&format!("\n\t\t{}", c));
                }
            }
            s.push(',');
            if let Some(idx) = index {
                v.push(idx);
            }
        }
        s.pop().unwrap();
        s.push_str("\n);");
        for idx in v {
            s.push_str(&format!("\n{}", idx));
        }
        s.push_str("\n\n");
        s
    }
}

pub trait ToQuery<D: DbType> {
    fn to_query(&self) -> String;
}

fn to_migration<D: DbPool>(v: &Vec<Box<dyn ToQuery<D>>>) -> String {
    let mut s = String::new();
    for d in v {
        s.push_str(&d.to_query());
    }
    s
}

pub async fn migrate<D: DbPool>(app_migrations: Vec<AppMigration<D>>, pool: &D) where <D::SqlRowVec as IntoIterator>::Item: DbRow {
    for app_migration in app_migrations {
        let mut migrations = vec![];
        let am = app_migration;
        let app = &am.0;
        for migration in &am.1 {
            migrations.push((migration.0.clone(), to_migration(&migration.1)));
        }
        println!("Checking {}", app);
        let val = format!("SELECT name FROM anansi_migrations WHERE app = '{}'", app);
        let rows = pool.raw_fetch_all(&val).await.unwrap();
        let mut names = vec![];
        for row in rows {
            let name = row.try_string("name").unwrap();
            names.push(name);
        }
        for (n, q) in migrations {
            if !names.contains(&n.to_string()) {
                println!("    Applying migration \"{}\"", n);
                let qs: Vec<&str> = q.split(';').collect();
                raw_transact! (pool, {
                    for s in qs {
                        pool.raw_execute(&format!("{};", s)).await?
                    }
                    Ok(())
                }).unwrap();
                
                let i = format!("INSERT INTO anansi_migrations (app, name, applied) VALUES('{app}', '{n}', {})", D::now());
                pool.raw_execute(&i).await.unwrap();
            }
        }
    }
}

pub async fn sql_migrate<D: DbPool>(app_migrations: Vec<AppMigration<D>>, app_name: &str, migration_name: &str) {
    for app_migration in app_migrations {
        let am = app_migration;
        if am.0 == app_name {
            for migration in &am.1 {
                if migration.0 == migration_name {
                    println!("{}", to_migration(&migration.1));
                    return;
                }
            }
        }
    }
}

pub async fn make_migrations<D: DbPool>(app_dir: &str, pool: &D) {
    let app_dir = PathBuf::from(app_dir);
    let mut v = vec![];
    let app_name = app_dir.file_name().unwrap().to_str().unwrap();
    let mut mfile = app_dir.clone();
    mfile.push("records.rs");
    let content = fs::read_to_string(&mfile).expect(&format!("could not open {}", mfile.to_str().unwrap()));
    process_syntax(app_name, content, &mut v);

    let mut syntaxes = Vec::new();
    let mut new_records = Vec::new();
    for (prefix, name, syntax) in v {
        let val = String::from(format!("SELECT schema FROM records WHERE name = '{}';\n", name));
        if let Ok(row) = pool.raw_fetch_one(&val).await {
            let s = row.try_string("schema").unwrap();
            if unescape(&s) == syntax {
                continue;
            } else {
                syntaxes.push((prefix, name, syntax));
            }
        } else {
            new_records.push((prefix, name, syntax));
        }
    }
    if !(new_records.is_empty() && syntaxes.is_empty()) {
        let mut sql = String::from("anansi::operations! {\n");
        new_syntax(&mut sql, new_records);
        add_syntax(&mut sql, syntaxes);
        
        sql.push_str("}");
        let row = pool.raw_fetch_one(&format!("SELECT COUNT(*) FROM anansi_migrations WHERE app = '{app_name}'")).await.unwrap();
        let n = row.try_count().unwrap();
        let mname = format!("{:04}", n+1);
        let mut mdir = app_dir.clone();
        mdir.push("migrations");
        let mut s = mdir.clone();
        s.push(&mname);
        fs::write(&s, sql).unwrap();
        println!("Created \"{}\"", s.to_str().unwrap());
        let mut idir = mdir.clone();
        idir.push("mod.rs");
        let original = fs::read_to_string(&idir).unwrap();
        if original.trim() == "use anansi::migrations::prelude::*;\n\nlocal_migrations! {}" {
            fs::write(idir, format!("use anansi::migrations::prelude::*;\n\nlocal_migrations! {{\n    \"{}\",\n}}", mname)).unwrap();
        } else {
            let split = original.rsplit_once('}').unwrap();
            fs::write(idir, format!("{}    \"{}\",\n}}", split.0, mname)).unwrap();
        }
    }
}

pub fn new_syntax(sql: &mut String, new_records: Vec<(String, String, String)>) {
    for (prefix, name, syntax) in new_records {
            sql.push_str(&format!("    migrations::CreateRecord {{\n        prefix: \"{}\",\n        name: \"{}\",\n        fields: vec![\n", prefix, name));
            sql.push_str(&syntax);
            sql.push_str("        ],\n    },\n");
        }
}

pub fn add_syntax(sql: &mut String, syntaxes: Vec<(String, String, String)>) {
    for (prefix, name, syntax) in syntaxes {
        let temp_prefix = format!("_{}", prefix);
        sql.push_str(&format!("    migrations::CreateRecord {{\n        prefix: \"{}\",\n        name: \"{}\",\n        fields: vec![\n", temp_prefix, name));
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
                let mut is_record = false;
                for attr in item.attrs {
                    if attr.path.segments.last().unwrap().ident.to_string() == "record" {
                        is_record = true;
                        break;
                    }
                }
                if !is_record {
                    continue;
                }
                match item.fields {
                    Named(named) => {
                        let mut meta = Vec::new();
                        for field in named.named {
                            let fieldname = field.ident.as_ref().unwrap().to_string();
                            if let Some(ty) = get_type(&fieldname, &field, &mut meta, db, &name) {
                                sql.push_str(&format!("            (\n                \"{}\",\n                records::{}\n            ),\n", fieldname, ty));
                            }
                        }
                        if !sql.contains("PRIMARY KEY") {
                            sql = "            (\n                \"id\",\n                records::BigInt::field().primary_key()\n            ),\n".to_string() + &sql;
                        }
                        v.push((prefix.to_string(), name.to_string(), sql));
                        v.push((prefix.to_string(), format!("{}tuple", name), "            (\n                \"id\",\n                records::BigInt::field().primary_key()\n            ),\n            (\n                \"subject_namespace\",\n                records::Text::field()\n            ),\n            (\n                \"subject_key\",\n                records::BigInt::field()\n            ),\n            (\n                \"subject_predicate\",\n                records::Text::field().null()\n            ),\n            (\n                \"object_key\",\n                records::BigInt::field()\n            ),\n            (\n                \"object_predicate\",\n                records::Text::field()\n            ),\n".to_string()));

                        if !alter {
                            key_table(meta, v, &prefix, &name);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            _ => {}
        }
    }
}

fn get_type(fieldname: &String, field: &Field, meta: &mut Vec<Vec<String>>, db: &str, name: &str) -> Option<String> {
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
                    }
                    "ManyToMany" => {
                        let mut v = Vec::new();
                        v.push(fieldname.clone());
                        v.push(segment.clone());
                        let s = parse_type(ty);
                        v.push(s);
                        meta.push(v);
                        return None;
                    }
                    "ForeignKey" => {
                        let mut s = String::new();
                        s.push_str("BigInt::field()");
                        let m = parse_type(ty);
                        let m: Vec<&str> = m.split(',').collect();
                        let m = m[0].trim().to_lowercase();
                        let parent: Vec<&str> = m.split("::").collect();
                        let parent_app = if let Some(p) = attrs.get("app") {
                            p.to_string()
                        } else {
                            format!("\"{}\"", db)
                        };
                        let parent_name = parent.last().unwrap().trim().to_string();
                        s.push_str(&format!(".foreign_key({}, \"{}\", \"id\")\n                    .index(\"{}_{}\", \"{}\")", parent_app, parent_name, db, name, fieldname));
                        s
                    }
                    "DateTime" => {
                        format!("DateTime::field()")
                    }
                    "Boolean" => {
                        format!("Boolean::field()")
                    }
                    "VarChar" => {
                        let n: u16 = parse_type(ty).parse().unwrap();
                        format!("VarChar::<{}>::field()", n)
                    }
                    "Text" => {
                        format!("Text::field()")
                    }
                    _ => {
                        unimplemented!()
                    }
                };
                if let Some(_) = attrs.get("unique") {
                    ty.push_str(".unique()");
                } else if let Some(value) = attrs.get("default") {
                    ty.push_str(&format!(".default({})", value));
                } else if let Some(_) = attrs.get("auto_now_add") {
                    ty.push_str(".auto_now_add()");
                }
                ty
            }
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
                sql.push_str(&format!("            (\n                \"{}\",\n                records::BigInt::field()", name));
                sql.push_str(&format!(".foreign_key(\"{}\", \"{}\", \"id\")\n            ),\n", prefix, name));
                sql.push_str(&format!("            (\n                \"{}\",\n                records::BigInt::field()", other));
                sql.push_str(&format!(".foreign_key(\"{}\", \"{}\", \"id\")\n            ),\n", prefix, other));
                v.push((prefix.to_string(), format!("{}_{}", name, other), sql));
            }
            _ => panic!("error populating table"),
        }
    }
}
