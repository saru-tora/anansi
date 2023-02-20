use std::str;
use std::sync::Arc;
use toml::Value::Table;
use async_trait::async_trait;
use tokio_postgres::types::ToSql;

use crate::try_sql;
use crate::server::Settings;
use crate::web::{Result, WebErrorKind, BaseRequest};
use crate::records::Record;
use crate::db::{DbRow, DbRowVec, DbPool, DbType, Builder, sql_stmt};

pub struct PgQuery<'q> {
    query: &'q str,
    params: &'q[&'q(dyn ToSql + Sync)],
}

impl<'q> PgQuery<'q> {
    pub fn new(query: &'q str, params: &'q[&'q(dyn ToSql + Sync)]) -> Self {
        Self {query, params}
    }
    pub async fn fetch_one<B: BaseRequest<SqlPool = PgDbPool>>(self, req: &B) -> Result<PgDbRow> {
        let statement = req.raw().pool().0.prepare(self.query).await?;
        Ok(PgDbRow {row: req.raw().pool().0.query_one(&statement, self.params).await?})
    }
    pub async fn fetch_all<B: BaseRequest<SqlPool = PgDbPool>>(self, req: &B) -> Result<PgDbRowVec> {
        let statement = req.raw().pool().0.prepare(self.query).await?;
        Ok(PgDbRowVec {rows: req.raw().pool().0.query(&statement, self.params).await?})
    }
    pub async fn execute<B: BaseRequest<SqlPool = PgDbPool>>(self, req: &B) -> Result<()> {
        let statement = req.raw().pool().0.prepare(self.query).await?;
        match req.raw().pool().0.execute(&statement, self.params).await {
            Ok(_) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
}

pub struct PgDbRow {
    row: tokio_postgres::row::Row,
}

impl DbRow for PgDbRow {
    type RawRow = tokio_postgres::row::Row;
    fn new(row: Self::RawRow) -> Self {
        Self {row}
    }
    fn try_bool(&self, index: &str) -> Result<bool> {
        try_sql!(self, index)
    }
    fn try_count(&self) -> Result<i64> {
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
        try_sql!(self, index)
    }
}

pub struct PgDbRowVec {
    rows: Vec<tokio_postgres::Row>,
}

impl DbRowVec for PgDbRowVec {
    type Row = PgDbRow;
    type Rows = Vec<tokio_postgres::Row>;
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
    row_into_iter: std::vec::IntoIter<tokio_postgres::Row>,
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
pub struct PgDbPool(pub(in crate) Arc<tokio_postgres::Client>);

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
                let row = p.query_one("SELECT EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'anansi_records');", &[]).await?;
                if !row.try_get(0)? {
                    p.execute("CREATE TABLE \"anansi_records\"(\n\t\"name\" text NOT NULL,\n\t\"schema\" text NOT NULL\n);", &[]).await?;
                    p.execute("CREATE TABLE \"anansi_migrations\"(\n\t\"id\" SERIAL PRIMARY KEY,\n\t\"app\" TEXT NOT NULL,\n\t\"name\" TEXT NOT NULL,\n\t\"applied\" TIMESTAMP NOT NULL\n);\n", &[]).await?;
                }
                Ok(Self(Arc::new(p)))
            }
            Err(e) => {
                Err(e)
            }
        }
    }
    async fn query(&self, val: &str) -> Result<PgDbRowVec> {
        Ok(PgDbRowVec {rows: self.0.query(val, &[]).await?})
    }
    async fn raw_fetch_one(&self, val: &str) -> Result<Self::SqlRow> {
        Ok(PgDbRow {row: self.0.query_one(val, &[]).await?})
    }
    async fn raw_fetch_all(&self, val: &str) -> Result<Self::SqlRowVec> {
        Ok(PgDbRowVec {rows: self.0.query(val, &[]).await?})
    }
    async fn raw_execute(&self, val: &str) -> Result<()> {
        match self.0.execute(val, &[]).await {
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
    async fn connect(arg: &str) -> Result<tokio_postgres::Client> {
        let (client, connection) = tokio_postgres::connect(arg, tokio_postgres::NoTls).await?;

        tokio::spawn(async move {
            if let Err(e) = connection.await {
                eprintln!("connection error: {}", e);
            }
        });
        Ok(client)
    }
}
