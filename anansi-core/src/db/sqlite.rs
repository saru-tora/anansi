use std::{str, thread, path::PathBuf};
use std::future::Future;
use tokio::process::Command;
use toml::Value::Table;
use async_trait::async_trait;
use sqlx::{Type, Database};
use sqlx::sqlite::Sqlite;

use crate::try_sql;
use crate::server::Settings;
use crate::records::Record;
use crate::web::{Result, BASE_DIR, WebErrorKind};
use crate::db::{Db, DbRow, DbRowVec, DbPool, DbType, Builder, sql_stmt};

#[derive(Clone)]
pub struct SqliteDb;

impl Db for SqliteDb {
    type SqlDb = sqlx::Sqlite;
    fn db_type_info<T: Type<Sqlite>>() -> <<Self as Db>::SqlDb as Database>::TypeInfo {
        <T as Type<Sqlite>>::type_info()
    }
}

pub struct SqliteDbRow {
    row: sqlx::sqlite::SqliteRow,
}

impl DbRow for SqliteDbRow {
    type SqlDb = Sqlite;
    type RawRow = sqlx::sqlite::SqliteRow;
    fn new(row: Self::RawRow) -> Self {
        Self {row}
    }
    fn try_bool(&self, index: &str) -> Result<bool> {
        try_sql!(self, index)
    }
    fn try_i32(&self, index: &str) -> Result<i32> {
        try_sql!(self, index)
    }
    fn try_i64(&self, index: &str) -> Result<i64> {
        try_sql!(self, index)
    }
    fn try_count(&self) -> Result<i64> {
        use sqlx::Row;
        match self.row.try_get(0) {
            Ok(o) => Ok(o),
            Err(e) => Err(Box::new(e))
        }
    }
    fn try_option_string(&self, index: &str) -> Result<Option<String>> {
        try_sql!(self, index)
    }
    fn try_string(&self, index: &str) -> Result<String> {
        try_sql!(self, index)
    }
    fn try_date_time(&self, index: &str) -> Result<String> {
        try_sql!(self, index)
    }
}

pub struct SqliteDbRowVec {
    rows: Vec<sqlx::sqlite::SqliteRow>,
}

impl DbRowVec for SqliteDbRowVec {
    type Row = SqliteDbRow;
    type Rows = Vec<sqlx::sqlite::SqliteRow>;
    fn from(rows: Self::Rows) -> Self {
        Self {rows}
    }
}

impl IntoIterator for SqliteDbRowVec {
    type Item = SqliteDbRow;
    type IntoIter = SqliteDbRowIntoIter;
    
    fn into_iter(self) -> Self::IntoIter {
        SqliteDbRowIntoIter {
            row_into_iter: self.rows.into_iter(),
        }
    }
}

pub struct SqliteDbRowIntoIter {
    row_into_iter: std::vec::IntoIter<sqlx::sqlite::SqliteRow>,
}

impl<'a> Iterator for SqliteDbRowIntoIter {
    type Item = SqliteDbRow;
    fn next(&mut self) -> Option<SqliteDbRow> {
        match self.row_into_iter.next() {
            Some(row) => Some(SqliteDbRow {row}),
            None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SqliteDbPool(pub(in crate) sqlx::Pool<Sqlite>);

#[async_trait]
impl DbPool for SqliteDbPool {
    type SqlRow = SqliteDbRow;
    type SqlRowVec = SqliteDbRowVec;
    async fn new(settings: &Settings) -> Result<Self> {
        let mut dir = PathBuf::new();
        BASE_DIR.with(|base| dir = base.clone());
        let databases = match settings.get("databases") {
            Some(v) => match v {
                Table(t) => t,
                _ => return Err(WebErrorKind::BadDb.to_box()),
            }
            None => return Err(WebErrorKind::BadDb.to_box()),
        };
        let name = match databases.get("default") {
            Some(d) => d.get("name").expect("Could not get database name").as_str().expect("Could not convert database name to string"),
            None => return Err(WebErrorKind::BadDb.to_box()),
        };
        dir.push(name);
        let ds = dir.to_str().unwrap();
        match Self::connect(ds).await {
            Ok(p) => Ok(Self {0: p}),
            Err(e) => {
                Self::init_db(ds).await;
                Err(e)
            }
        }
    }
    async fn transact<F: Future<Output = Result<O>> + Send, O: Send>(&self, future: F) -> F::Output {
        let tran = self.0.begin().await?;
        let res = future.await;
        if res.is_ok() {
            tran.commit().await?;
        }
        res
    }
    async fn query(&self, val: &str) -> Result<SqliteDbRowVec> {
        Ok(SqliteDbRowVec {rows: sqlx::query(val).fetch_all(&self.0).await?})
    }
    async fn raw_fetch_one(&self, val: &str) -> Result<Self::SqlRow> {
        Ok(Self::SqlRow {row: sqlx::query(val).fetch_one(&self.0).await?})
    }
    async fn raw_fetch_all(&self, val: &str) -> Result<Self::SqlRowVec> {
        Ok(Self::SqlRowVec {rows: sqlx::query(val).fetch_all(&self.0).await?})
    }
    async fn raw_execute(&self, val: &str) -> Result<()> {
        match sqlx::query(val).execute(&self.0).await {
            Ok(_) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
    fn now() -> &'static str {
        "strftime('%Y-%m-%d %H-%M-%f','now')"
    }
    async fn test() -> Result<Self> {
        let pool = sqlx::sqlite::SqlitePoolOptions::new()
            .max_connections(thread::available_parallelism().unwrap().get() as u32)
            .connect(":memory:").await?;
        sqlx::query(INIT_STR).execute(&pool).await?;
        let pool = Self {0: pool};
        Ok(pool)
    }
    fn to_stmt<R: Record>(val: Builder<R>) -> String {
        sql_stmt(val)
    }
}

impl DbType for SqliteDbPool {
    fn db_type(ty: &str) -> String {
        ty.to_string()
    }
}

static INIT_STR: &str = "CREATE TABLE \"anansi_records\"(\n\t\"name\" text NOT NULL,\n\t\"schema\" text NOT NULL\n);\nCREATE TABLE anansi_migrations(\n\t\"id\" INT PRIMARY KEY,\n\t\"app\" TEXT NOT NULL,\n\t\"name\" TEXT NOT NULL,\n\t\"applied\" DATETIME NOT NULL\n);\n";

impl SqliteDbPool {
    async fn connect(dir: &str) -> Result<sqlx::Pool<Sqlite>> {
        sqlx::sqlite::SqlitePoolOptions::new()
            .max_connections(thread::available_parallelism().unwrap().get() as u32)
            .connect(&dir).await.or(Err(WebErrorKind::BadDb.to_box()))
    }

    async fn init_db(dir: &str) {
        let mut cmd = Command::new("sqlite3");
        cmd.arg(dir);
        cmd.arg(INIT_STR);
        let mut child = cmd.spawn().expect("Failed to start sqlite3");
        child.wait().await.expect("failed to wait on child");
        println!("Initialized database");
    }
}
