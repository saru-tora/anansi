use std::{str, thread};
use std::future::Future;
use toml::Value::Table;
use async_trait::async_trait;
use sqlx::{Type, Database};
use sqlx::types::chrono::NaiveDateTime;
use sqlx::postgres::Postgres;

use crate::try_sql;
use crate::server::Settings;
use crate::web::{Result, WebErrorKind};
use crate::records::Record;
use crate::db::{Db, DbRow, DbRowVec, DbPool, DbType, Builder, sql_stmt};

#[derive(Clone)]
pub struct PgDb;

impl Db for PgDb {
    type SqlDb = sqlx::Postgres;
    fn db_type_info<T: Type<Postgres>>() -> <<Self as Db>::SqlDb as Database>::TypeInfo {
        <T as Type<Postgres>>::type_info()
    }
}

pub struct PgDbRow {
    row: sqlx::postgres::PgRow,
}

impl DbRow for PgDbRow {
    type SqlDb = Postgres;
    type RawRow = sqlx::postgres::PgRow;
    fn new(row: Self::RawRow) -> Self {
        Self {row}
    }
    fn try_bool(&self, index: &str) -> Result<bool> {
        try_sql!(self, index)
    }
    fn try_count(&self) -> Result<i64> {
        use sqlx::Row;
        match self.row.try_get(0) {
            Ok(c) => Ok(c),
            Err(e) => {
                Err(Box::new(e))
            }
        }
    }
    fn try_i32(&self, index: &str) -> Result<i32> {
        let index: &str = &(index.to_lowercase());
        try_sql!(self, index)
    }
    fn try_i64(&self, index: &str) -> Result<i64> {
        try_sql!(self, index)
    }
    fn try_option_string(&self, index: &str) -> Result<Option<String>> {
        try_sql!(self, index)
    }
    fn try_string(&self, index: &str) -> Result<String> {
        try_sql!(self, index)
    }
    fn try_date_time(&self, index: &str) -> Result<String> {
        use sqlx::Row;
        let dt: NaiveDateTime = match self.row.try_get(index) {
            Ok(s) => s,
            Err(e) => {
                return Err(Box::new(e));
            }
        };
        Ok(dt.to_string())
    }
}

pub struct PgDbRowVec {
    rows: Vec<sqlx::postgres::PgRow>,
}

impl DbRowVec for PgDbRowVec {
    type Row = PgDbRow;
    type Rows = Vec<sqlx::postgres::PgRow>;
    fn from(rows: Self::Rows) -> Self {
        Self {rows}
    }
}

impl IntoIterator for PgDbRowVec {
    type Item = PgDbRow;
    type IntoIter = PgDbRowIntoIter;
    
    fn into_iter(self) -> Self::IntoIter {
        PgDbRowIntoIter {
            row_into_iter: self.rows.into_iter(),
        }
    }
}

pub struct PgDbRowIntoIter {
    row_into_iter: std::vec::IntoIter<sqlx::postgres::PgRow>,
}

impl<'a> Iterator for PgDbRowIntoIter {
    type Item = PgDbRow;
    fn next(&mut self) -> Option<PgDbRow> {
        match self.row_into_iter.next() {
            Some(row) => Some(PgDbRow {row}),
            None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PgDbPool(pub(in crate) sqlx::Pool<Postgres>);

#[async_trait]
impl DbPool for PgDbPool {
    type SqlRow = PgDbRow;
    type SqlRowVec = PgDbRowVec;
    async fn new(settings: &Settings) -> Result<Self> {
        let databases = match settings.get("databases") {
            Some(v) => match v {
                Table(t) => t,
                _ => return Err(WebErrorKind::BadDb.to_box()),
            }
            None => return Err(WebErrorKind::BadDb.to_box()),
        };
        let database = databases.get("default").expect("Could not get database information");
        let name = database.get("name").expect("Could not get database name").as_str().expect("Could not convert database name to string");
        let user = database.get("user").expect("Could not get database user").as_str().expect("Could not convert database user to string");
        let password = database.get("password").expect("Could not get database password").as_str().expect("Could not convert database password to string");
        let address = database.get("address").expect("Could not get database host").as_str().expect("Could not convert database host to string");
        let arg = format!("postgres://{user}:{password}@{address}/{name}");
        match Self::connect(&arg).await {
            Ok(p) => {
                use sqlx::Row;

                let row = sqlx::query("SELECT EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'anansi_records');").fetch_one(&p).await?;
                if !row.try_get(0)? {
                    sqlx::query("CREATE TABLE \"anansi_records\"(\n\t\"name\" text NOT NULL,\n\t\"schema\" text NOT NULL\n);").execute(&p).await?;
                    sqlx::query("CREATE TABLE \"anansi_migrations\"(\n\t\"id\" SERIAL PRIMARY KEY,\n\t\"app\" TEXT NOT NULL,\n\t\"name\" TEXT NOT NULL,\n\t\"applied\" TIMESTAMP NOT NULL\n);\n").execute(&p).await?;
                }
                Ok(Self {0: p})
            }
            Err(e) => {
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
    async fn query(&self, val: &str) -> Result<PgDbRowVec> {
        Ok(PgDbRowVec {rows: sqlx::query(val).fetch_all(&self.0).await?})
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
        "NOW()"
    }
    async fn test() -> Result<Self> {
        unimplemented!()
    }
    fn to_stmt<R: Record>(val: Builder<R>) -> String {
        sql_stmt(val)
    }
}

impl DbType for PgDbPool {
    fn db_type(ty: &str) -> String {
        if ty == "datetime" {
            "timestamp".to_string()
        } else {
            ty.to_string()
        }
    }
}

impl PgDbPool {
    async fn connect(arg: &str) -> Result<sqlx::Pool<Postgres>> {
        let pg = sqlx::postgres::PgPoolOptions::new()
            .max_connections(thread::available_parallelism().unwrap().get() as u32)
            .connect(arg).await;
        match pg {
            Ok(pg) => Ok(pg),
            Err(e) => {
                Err(Box::new(e))
            }
        }
    }
}
