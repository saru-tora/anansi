use std::{str, fs};
use std::collections::HashMap;
use quote::quote;
use syn::Type;
use syn::Field;
use syn::Item::Struct;
use syn::Fields::Named;
use syn::Attribute;
use sqlx::Row;
use crate::db::{DbPool, unescape};

#[macro_export]
macro_rules! apps {
    ($($name:ident,)*) => {
        static APP_MIGRATIONS: &[anansi::syntax::AppMigration] = &[
            $($name::migrations::init::MIGRATIONS,)*
        ];
    }
}

#[macro_export]
macro_rules! migrations {
    ($($name:literal,)*) => {
        pub static MIGRATIONS: anansi::syntax::AppMigration = (
            super::super::init::APP_NAME, &[
            $(($name, include_bytes!($name)),)*
        ]);
    }
}

pub type AppMigration = (&'static str, &'static [Migration]);
pub type Migration = (&'static str, &'static [u8]);

pub async fn migrate(app_migrations: &[AppMigration], pool: &DbPool) {
    for (app, migrations) in app_migrations {
        println!("Checking {}", app);
        let rows = sqlx::query("SELECT name FROM anansi_migrations WHERE app = ?").bind(app).fetch_all(&pool.0).await.unwrap();
        let mut names = vec![];
        for row in rows {
            let name: String = row.try_get("name").unwrap();
            names.push(name);
        }
        for (n, q) in *migrations {
            if !names.contains(&n.to_string()) {
                println!("    Applying migration \"{}\"", n);
                sqlx::query(str::from_utf8(q).unwrap()).execute(&pool.0).await.unwrap();
                sqlx::query("INSERT INTO anansi_migrations (app, name, applied) VALUES(?, ?, strftime('%Y-%m-%d %H-%M-%f','now'))").bind(app).bind(n).execute(&pool.0).await.unwrap();
            }
        }
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
    for (name, syntax) in v {
        let val = String::from(format!("SELECT schema FROM models WHERE name = '{}';\n", name));
        if let Ok(row) = sqlx::query(&val).fetch_one(&pool.0).await {
            let s: &str = row.try_get("schema").unwrap();
            if unescape(&s) == syntax {
                continue;
            } else {
                syntaxes.push((name, syntax));
            }
        } else {
            new_models.push((name, syntax));
        }
    }
    if !(new_models.is_empty() && syntaxes.is_empty()) {
        let mut sql = String::from("BEGIN;\n\n");
        for (name, syntax) in new_models {
            sql.push_str(&format!("CREATE TABLE \"{}\" (\n", name));
            sql.push_str(&syntax);
            sql.push_str("\n);\n\n");
        }
        for (name, syntax) in syntaxes {
            let temp_name = format!("_{}", name);
            sql.push_str(&format!("CREATE TABLE \"{}\" (\n", temp_name));
            sql.push_str(&syntax);
            sql.push_str("\n);\n\n");
            sql.push_str(&format!("INSERT INTO \"{}\" SELECT * FROM \"{}\";\n", temp_name, name));
            sql.push_str(&format!("DROP TABLE \"{}\";\n", name));
            sql.push_str(&format!("ALTER TABLE \"{}\" RENAME TO \"{}\";\n\n", temp_name, name));
        }
        sql.push_str("COMMIT;");
        let row = sqlx::query("SELECT COUNT(*) as count FROM anansi_migrations WHERE app = ?").bind(app_name).fetch_one(&pool.0).await.unwrap();
        let n: u16 = row.try_get("count").unwrap();
        let mname = format!("{:04}", n+1);
        let mdir = format!("{}migrations/", app_dir);
        let s = format!("{}{}", mdir, mname);
        fs::write(&s, sql).unwrap();
        println!("Created \"{}\"", s);
        let idir = format!("{}init.rs", mdir);
        let original = fs::read_to_string(&idir).unwrap();
        if original.trim() == "anansi::migrations! {}" {
            fs::write(idir, format!("anansi::migrations! {{\n    \"{}\",\n}}", mname)).unwrap();
        } else {
            let split = original.rsplit_once('}').unwrap();
            fs::write(idir, format!("{}    \"{}\",\n}}", split.0, mname)).unwrap();
        }
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

pub fn process_syntax(db: &str, content: String, v: &mut Vec<(String, String)>) {
    let alter = false;
    let syntax = syn::parse_file(&content).expect("Unable to parse file");
    for item in syntax.items {
        match item {
            Struct(item) => {
                let mut sql = String::new();
                let name = item.ident.to_string().to_lowercase();
                let new_name = if !alter {
                    format!("{}_{}", db, name)
                } else {
                    format!("_{}_{}", db, name)
                };
                let mut n = 0;
                match item.fields {
                    Named(named) => {
                        let mut meta = Vec::new();
                        for field in named.named {
                            let fieldname = field.ident.as_ref().unwrap().to_string();
                            if let Some(ty) = get_type(&fieldname, &field, &mut meta) {
                                if n > 0 {
                                    sql.push_str(",\n");
                                }
                                sql.push_str(&format!("\t\"{}\" {}", fieldname, ty));
                            }
                            n += 1;
                        }
                        if !sql.contains("PRIMARY KEY") {
                            sql = "\t\"id\" NOT NULL PRIMARY KEY,\n".to_string() + &sql;
                        }
                        let mut m2 = Vec::new();
                        for m in meta {
                            if m[1] == "ForeignKey" {
                                if n > 0 {
                                    sql.push_str(",\n");
                                }
                                let mut s = String::new();
                                s.push_str(&format!("\tFOREIGN KEY (\"{}\")\n", m[0]));
                                let parent: Vec<&str> = m[2].split("::").collect();
                                let parent_app = if parent.len() > 2 {
                                    parent[parent.len() - 3].trim().to_string()
                                } else {
                                    db.to_string()
                                };
                                let parent_name = parent.last().unwrap().trim().to_string();
                                s.push_str(&format!("\tREFERENCES \"{}_{}\" (\"id\")\n", parent_app, parent_name));
                                s.push_str(&format!("\tON DELETE CASCADE"));
                                sql.push_str(&s);
                                n += 1;
                            } else {
                                m2.push(m);
                            }
                        }
                        v.push((new_name, sql));
                        if !alter {
                            key_table(m2, v, db, &name);
                        }
                    },
                    _ => unimplemented!(),
                }
            },
            _ => {},
        }
    }
}

fn get_type(fieldname: &String, field: &Field, meta: &mut Vec<Vec<String>>) -> Option<String> {
    let ty = &field.ty;
    Some(
        match ty {
            Type::Path(path) => {
                let segment = path.path.segments[0].ident.to_string();
                let attrs = get_attrs(&field.attrs);
                let mut ty = match segment.as_str() {
                    "BigInt" => {
                        let mut s = "bigint NOT NULL".to_owned();
                        if let Some(pk) = attrs.get("primary_key") {
                            if pk == "true" {
                                s.push_str(" PRIMARY KEY");
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
                        let mut v = Vec::new();
                        v.push(fieldname.clone());
                        v.push(segment.clone());
                        let s = parse_type(ty);
                        let s: Vec<&str> = s.split(',').collect();
                        v.push(s[0].trim().to_lowercase());
                        meta.push(v);
                        "bigint NOT NULL".to_owned()
                    },
                    "DateTime" => {
                        "datetime NOT NULL".to_owned()
                    },
                    "Boolean" => {
                        "boolean NOT NULL".to_owned()
                    },
                    "VarChar" => {
                        let n: u16 = parse_type(ty).parse().unwrap();
                        format!("varchar({}) NOT NULL", n)
                    },
                    "Text" => {
                        "text NOT NULL".to_owned()
                    },
                    _ => unimplemented!(),
                };
                if let Some(_) = attrs.get("unique") {
                    ty.push_str(" UNIQUE");
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

fn key_table(m2: Vec<Vec<String>>, v: &mut Vec<(String, String)>, db: &str, name: &str) {
    for m in m2 {
        match m[1].as_str() {
            "ManyToMany" => {
                let other = m[2].to_lowercase();
                let mut sql = String::new();
                sql.push_str(&format!("\t\"{}_id\" bigint NOT NULL,\n", name));
                sql.push_str(&format!("\t\"{}_id\" bigint NOT NULL,\n", other));
                sql.push_str(&format!("\tFOREIGN KEY (\"{}_id\")\n", name));
                sql.push_str(&format!("\tREFERENCES \"{}_{1}\" (\"id\")\n", db, name));
                sql.push_str("\tON DELETE CASCADE,\n");
                sql.push_str(&format!("\tFOREIGN KEY (\"{}_id\")\n", other));
                sql.push_str(&format!("\tREFERENCES \"{}_{1}\" (\"id\")\n", db, other));
                sql.push_str("\tON DELETE CASCADE");
                v.push((format!("{}_{}_{}", db, name, other), sql));
            },
            _ => panic!("error populating table"),
        }
    }
}
