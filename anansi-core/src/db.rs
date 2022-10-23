use std::{str, thread};
use std::io::{self, Read, ErrorKind};
use std::marker::PhantomData;
use std::borrow::Cow;
use std::future::Future;
use tokio::process::Command;

use sqlx::{Decode, Type};

use crate::records::{Record, DataType, BigInt, Objects, ToSql};
use crate::web::{Result, BaseRequest, BASE_DIR};

pub type Db = sqlx::Sqlite;
pub type DbTypeInfo = sqlx::sqlite::SqliteTypeInfo;
type RawRow = sqlx::sqlite::SqliteRow;

pub struct DbRow {
    row: sqlx::sqlite::SqliteRow,
}

impl DbRow {
    pub fn try_get<'r, T>(&'r self, index: &str) -> Result<T>
        where T: Decode<'r, Db> + Type<Db>
    {
        use sqlx::Row;
        self.row.try_get(index).or(Err(invalid()))
    }
}

pub struct DbRowVec {
    rows: Vec<sqlx::sqlite::SqliteRow>,
}

impl IntoIterator for DbRowVec {
    type Item = DbRow;
    type IntoIter = DbRowIntoIter;
    
    fn into_iter(self) -> Self::IntoIter {
        DbRowIntoIter {
            row_into_iter: self.rows.into_iter(),
        }
    }
}

pub struct DbRowIntoIter {
    row_into_iter: std::vec::IntoIter<RawRow>,
}

impl<'a> Iterator for DbRowIntoIter {
    type Item = DbRow;
    fn next(&mut self) -> Option<DbRow> {
        match self.row_into_iter.next() {
            Some(row) => Some(DbRow {row}),
            None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DbPool(pub(in crate) sqlx::Pool<Db>);

impl DbPool {
    pub async fn new() -> Result<Self> {
        let mut dir = String::new();
        BASE_DIR.with(|base| dir = format!("{}/{}", base, "database.db"));
        match Self::connect(&dir).await {
            Ok(p) => Ok(Self {0: p}),
            Err(e) => {
                Self::init_db(&dir).await;
                Err(e)
            },
        }
    }
    async fn connect(dir: &str) -> Result<sqlx::Pool<Db>> {
        sqlx::sqlite::SqlitePoolOptions::new()
            .max_connections(thread::available_parallelism().unwrap().get() as u32)
            .connect(&dir).await.or(Err(invalid()))
    }
    async fn init_db(dir: &str) {
        let mut cmd = Command::new("sqlite3");
        cmd.arg(dir);
        cmd.arg("CREATE TABLE \"anansi_records\"(\n\t\"name\" text NOT NULL,\n\t\"schema\" text NOT NULL\n);\nCREATE TABLE anansi_migrations(\n\t\"id\" INT PRIMARY KEY,\n\t\"app\" TEXT NOT NULL,\n\t\"name\" TEXT NOT NULL,\n\t\"applied\" DATETIME NOT NULL\n);\n");
        let mut child = cmd.spawn().expect("Failed to start cargo");
        child.wait().await.expect("failed to wait on child");
        println!("Initialized database");
    }
    pub async fn transact<F: Future<Output = Result<O>>, O>(&self, future: F) -> F::Output {
        let tran = self.0.begin().await?;
        let res = future.await;
        if res.is_ok() {
            tran.commit().await?;
        }
        res
    }
    pub async fn query(&self, val: &str) -> Result<DbRowVec> {
        Ok(DbRowVec {rows: sqlx::query(val).fetch_all(&self.0).await?})
    }
}

const NEWLINE: u8 = 10;
const RETURN: u8 = 13;
const DOUBLE_QUOTE: u8 = 34;
const COMMA: u8 = 44;

pub struct Buffer {
    pos: usize,
    buf: Vec<u8>,
}

impl Buffer {
    pub fn new() -> Self {
        Self {pos: 0, buf: vec![]}
    }
    pub fn to_s(self, l: usize) -> String {
        String::from_utf8(self.buf[l..].to_vec()).unwrap()
    }
    pub fn from<R: Read>(reader: &mut R) -> Result<Self> {
        let mut buf = vec![0; 1024];
        let len = reader.read(&mut buf)?;
        if len < buf.len() {
            buf.truncate(buf.len()-len);
            Ok(Self {pos: 0, buf})
        } else {
            Err(invalid())
        }
    }
    pub fn split_comma(&mut self) -> Result<Vec<Cow<'_, str>>> {
        let mut v = vec![];
        if !self.buf.is_empty() {
            let mut iter = self.buf.iter();
            while let Some(b) = iter.next() {
                self.pos += 1;
                if *b == NEWLINE {
                    break;
                }
            }
            let mut m = self.pos;
            loop {
                if let Some(b) = iter.next() {
                    if *b != DOUBLE_QUOTE {
                        while let Some(c) = iter.next() {
                            let c = *c;
                            m += 1;
                            if c == COMMA || c == RETURN {
                                v.push(Cow::from(str::from_utf8(&self.buf[self.pos..m])?));
                                if c == RETURN {
                                    while let Some(d) = iter.next() {
                                        let d = *d;
                                        m += 1;
                                        if d == NEWLINE {
                                            break;
                                        }
                                    }
                                }
                                m += 1;
                                self.pos = m;
                                break;
                            }
                        }
                    } else {
                        let mut bv = vec![];
                        while let Some(c) = iter.next() {
                            let c = *c;
                            if c == DOUBLE_QUOTE {
                                if let Some(d) = iter.next() {
                                    m += 1;
                                    let d = *d;
                                    if d != DOUBLE_QUOTE {
                                        m += bv.len() + 1;
                                        v.push(Cow::from(String::from_utf8(bv)?));
                                        if d != COMMA {
                                            while let Some(e) = iter.next() {
                                                let e = *e;
                                                m += 1;
                                                if e == NEWLINE {
                                                    break;
                                                }
                                            }
                                        }
                                        m += 1;
                                        self.pos = m;
                                        break;
                                    }
                                }
                            }
                            bv.push(c);
                        }
                    }
                } else {
                    break;
                }
            }
        }
        Ok(v)
    }
}

pub fn invalid() -> Box<io::Error> {
    Box::new(io::Error::from(ErrorKind::InvalidData))
}

pub fn percent_escape(s: &str) -> String {
    let mut t = String::new();
    inner_escape(s, &mut t);
    let mut val = String::new();
    for c in t.chars() {
        if c != '%' {
            val.push(c);
        } else {
            val.push_str("%%");
        }
    }
    val
}

pub fn escape(s: &str) -> String {
    let mut val = String::from("'");
    inner_escape(s, &mut val);
    val.push('\'');
    val
}

fn inner_escape(s: &str, val: &mut String) {
    for c in s.chars() {
        if c != '\'' {
            val.push(c);
        } else {
            val.push_str("''");
        }
    }
}

pub fn unescape(s: &str) -> String {
    let mut val = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\0' {
            break;
        } else if c != '"' {
            val.push(c);
        } else {
            val.push(chars.next().unwrap());
        }
    }
    val.pop().unwrap();
    val.pop().unwrap();
    val
}

struct Statement<S: Record> {
    val: Builder<S>,
}

#[derive(Debug)]
pub struct Builder<B: Record> {
    start: String,
    join: String,
    val: String,
    m: PhantomData<B>,
}

impl<B: Record> Clone for Builder<B> {
    fn clone(&self) -> Self {
        Self {start: self.start.clone(), join: self.join.clone(), val: self.val.clone(), m: PhantomData.clone()}
    }
}

impl<B: Record> Builder<B> {
    pub fn new() -> Self {
        Self {start: String::new(), join: String::new(), val: String::new(), m: PhantomData}
    }
    fn from(start: String) -> Self {
        Self {start, join: String::new(), val: String::new(), m: PhantomData}
    }
    pub fn count(database: &str) -> Self {
        let start = format!("SELECT COUNT(*) as count FROM {}", database);
        Self::from(start)
    }
    pub fn select(columns: &[&str], database: &str) -> Self {
        let mut start = format!("SELECT {}.{}", database, columns[0]);
        if columns.len() > 1 {
            for column in &columns[1..] {
                start.push_str(&format!(", {}.{}", database, column));
            }
        }
        start.push_str(&format!(" FROM {}", database));
        Self::from(start)
    }
    pub fn insert_into(database: &str, columns: &[&str])  -> Self {
        let mut start = format!("INSERT INTO {} ({}", database, columns[0]);
        if columns.len() > 1 {
            for column in &columns[1..] {
                start.push_str(&format!(", {}", column));
            }
        }
        start.push_str(") VALUES (");
        Self::from(start)
    }
    pub fn delete(database: &str)  -> Self {
        let start = format!("DELETE FROM {}", database);
        Self::from(start)
    }
    pub fn update(database: &str)  -> Self {
        let start = format!("UPDATE {} SET", database);
        Self::from(start)
    }
    pub fn push(&mut self, s: &str) {
        self.val.push_str(s);
    }
    pub fn push_str(mut self, s: &str) -> Self {
        self.val.push_str(s);
        self
    }
    pub fn append(mut self, other: Self) -> Self {
        self.val.push_str(&other.val);
        self.join.push_str(&other.join);
        self
    }
    pub fn and(mut self) -> Self {
        self.val.push_str(" AND ");
        self
    }
    pub fn comma(mut self) -> Self {
        self.val.push_str(" , ");
        self
    }
    pub fn whose(mut self) -> Self {
        self.val.push_str(" WHERE ");
        self
    }
    pub fn column(mut self, c: &str) -> Self {
        self.val.push_str(c);
        self
    }
    pub fn inner_join(mut self, t1: &str, t2: &str, c: &str, d: &str) -> Self {
        let s = format!(" INNER JOIN {} ON {1}.{2} = {0}.{3}", t1, t2, c, d);
        self.join.push_str(&s);
        self
    }
    pub fn order_by(mut self) -> Self {
        self.val.push_str(" ORDER BY ");
        self
    }
    pub fn limit(mut self, n: u32) -> Self {
        self.val.push_str(&format!(" LIMIT {}", n));
        self
    }
    pub fn val(self) -> String {
        self.start + &self.join + &self.val
    }
}

pub struct DeleteWhoseArg<M: Record> {
    b: Builder<M>,
}

impl<M: Record> DeleteWhoseArg<M> {
    pub fn builder(self) -> Builder<M> {
        self.b
    }
}
pub struct WhoseArg<M: Record> {
    b: Builder<M>,
}

impl<M: Record> WhoseArg<M> {
    pub fn from(b: Builder<M>) -> Self {
        Self {b}
    }
    pub fn builder(self) -> Builder<M> {
        self.b
    }
}

pub struct Count<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> Count<M> {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub fn whose(self, w: WhoseArg<M>) -> WhoseCount<M> {
        WhoseCount::from(self.stmt.val.whose().append(w.b))
    }
    pub fn by_pk<D: DataType + std::cmp::PartialEq<<D as DataType>::T>>(self, pks: &Vec<D>, fk: Column<M, D>) -> LimitCount<M> {
        self.whose(fk.clone().is_in(pks)).group_by(fk.clone()).order_by(fk.field(pks)).limit(pks.len() as u32)
    }
}

pub struct WhoseCount<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> WhoseCount<M>  {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub fn group_by<D: DataType>(self, arg: Column<M, D>) -> GroupByCount<M> {
        self.stmt.group_by_count(arg)
    }
    pub fn order_by(self, arg: OrderByArg<M>) -> OrderByCount<M> {
        self.stmt.order_by_count(arg)
    }
    pub fn and(self, arg: WhoseArg<M>) -> Self {
        Self{stmt: self.stmt.and(arg.b)}
    }
    pub async fn raw_get(self, pool: &DbPool) -> Result<u32> {
        self.stmt.raw_get_count(pool).await
    }
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<u32> {
        self.stmt.get_count(req).await
    }
    pub fn limit(self, n: u32) -> LimitCount<M> {
        self.stmt.limit_count(n)
    }
}

pub struct DeleteWhose<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> DeleteWhose<M>  {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub async fn execute<B: BaseRequest>(self, req: &B) -> Result<()> {
        if !req.raw().valid_token() {
            return Err(invalid());
        }
        let mut val = self.stmt.val.val();
        val.push_str(";\n");
       
        match sqlx::query(&val).execute(&req.raw().pool().0).await {
            Ok(_) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
}

pub struct Whose<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> Whose<M>  {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub fn order_by(self, arg: OrderByArg<M>) -> OrderBy<M> {
        self.stmt.order_by(arg)
    }
    pub fn and(self, arg: WhoseArg<M>) -> Self {
        Self{stmt: self.stmt.and(arg.b)}
    }
    pub fn or(self, arg: WhoseArg<M>) -> Self {
        Self{stmt: self.stmt.or(arg.b)}
    }
    pub async fn raw_get(self, pool: &DbPool) -> Result<M> {
        self.stmt.raw_get(pool).await
    }
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<M> {
        self.stmt.get(req).await
    }
    pub fn get_all(self) -> Limit<M> {
        self.stmt.get_all()
    }
    pub fn limit(self, n: u32) -> Limit<M> {
        self.stmt.limit(n)
    }
}

pub struct OrderByArg<M: Record> {
    b: Builder<M>,
}

impl<M: Record> OrderByArg<M> {
    pub fn builder(self) -> Builder<M> {
        self.b
    }
}

pub struct OrderBy<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> OrderBy<M> {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<M> {
        self.stmt.get(req).await
    }
    pub fn limit(self, n: u32) -> Limit<M> {
        self.stmt.limit(n)
    }
}

pub struct GroupByCount<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> GroupByCount<M> {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub fn order_by(self, arg: OrderByArg<M>) -> OrderByCount<M> {
        self.stmt.order_by_count(arg)
    }
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<u32> {
        self.stmt.get_count(req).await
    }
    pub fn limit(self, n: u32) -> LimitCount<M> {
        self.stmt.limit_count(n)
    }
}

pub struct OrderByCount<M: Record> {
    stmt: Statement<M>,
}

impl<M: Record> OrderByCount<M> {
    pub fn from(b: Builder<M>) -> Self {
        Self {stmt: Statement::from(b)}
    }
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<u32> {
        self.stmt.get_count(req).await
    }
    pub fn limit(self, n: u32) -> LimitCount<M> {
        self.stmt.limit_count(n)
    }
}

impl<S: Record> Statement<S> {
    fn from(val: Builder<S>) -> Self {
        Self {val}
    }
    fn order_by(self, arg: OrderByArg<S>) -> OrderBy<S> {
        OrderBy::from(self.val.push_str(&format!(" ORDER BY {}", arg.b.val())))
    }
    fn order_by_count(self, arg: OrderByArg<S>) -> OrderByCount<S> {
        OrderByCount::from(self.val.push_str(&format!(" ORDER BY {}", arg.b.val())))
    }
    fn group_by_count<D: DataType>(self, arg: Column<S, D>) -> GroupByCount<S> {
        GroupByCount::from(self.val.push_str(&format!(" GROUP BY {}", arg.b.val)))
    }
    fn and(mut self, val: Builder<S>) -> Self {
        let s = format!(" AND {}", val.val);
        self.val.val.push_str(&s);
        self.val.join.push_str(&val.join);
        self
    }
    fn or(mut self, val: Builder<S>) -> Self {
        let s = format!(" OR {}", val.val);
        self.val.val.push_str(&s);
        self.val.join.push_str(&val.join);
        self
    }
    pub async fn raw_get_count(self, pool: &DbPool) -> Result<u32> {
        use sqlx::Row;
        let mut val = self.val.val();
        val.push_str(";\n");

        match sqlx::query(&val).fetch_one(&pool.0).await {
            Ok(row) => Ok(row.try_get("count")?),
            Err(_) => Err(invalid()),
        }
    }
    async fn get_count<B: BaseRequest>(self, req: &B) -> Result<u32> {
        self.raw_get_count(req.raw().pool()).await
    }
    async fn raw_get(self, pool: &DbPool) -> Result<S> {
        let mut val = self.val.val();
        val.push_str(";\n");

        match sqlx::query(&val).fetch_one(&pool.0).await {
            Ok(row) => S::get(DbRow{row}),
            Err(_) => Err(invalid()),
        }
    }
    async fn get<B: BaseRequest>(self, req: &B) -> Result<S> {
        self.raw_get(req.raw().pool()).await
    }
    fn get_all(self) -> Limit<S> {
        Limit::from(self.val)
    }
    fn limit(self, n: u32) -> Limit<S> {
        Limit::from(self.val.limit(n))
    }
    fn limit_count(self, n: u32) -> LimitCount<S> {
        LimitCount::from(self.val.limit(n))
    }
}

pub struct Column<M: Record, T: ToSql> {
    b: Builder<M>,
    t: PhantomData<T>,
}

impl<M: Record, T: ToSql> Clone for Column<M, T> {
    fn clone(&self) -> Self {
        Self {b: self.b.clone(), t: self.t.clone()}
    }
}

impl<M: Record, D: DataType<T = String>> Column<M, D> {
    pub fn contains<'a, U: ToSql + PartialEq<&'a str>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" LIKE '%{}%'", percent_escape(&u.to_sql()))))
    }
    pub fn icontains<'a, U: ToSql + std::fmt::Display + PartialEq<&'a str>>(mut self, u: U) -> WhoseArg<M> {
        self.b.val = format!("LOWER({}) LIKE LOWER('%{}%')", self.b.val, percent_escape(&u.to_string()));
        WhoseArg::from(self.b)
    }
    pub fn iexact<'a, U: ToSql + PartialEq<&'a str>>(mut self, u: U) -> WhoseArg<M> {
        self.b.val = format!("LOWER({}) = LOWER({})", self.b.val, u.to_sql());
        WhoseArg::from(self.b)
    }
    pub fn starts_with<'a, U: ToSql + PartialEq<&'a str>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" LIKE \"{}%\"", percent_escape(&u.to_sql()))))
    }
    pub fn ends_with<'a, U: ToSql + PartialEq<&'a str>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" LIKE \"%{}\"", percent_escape(&u.to_sql()))))
    }
}

impl<M: Record, D: DataType> Column<M, D> {
    pub fn new(s: &str) -> Self {
        let b = Builder::new().push_str(s);
        Self {b, t: PhantomData}
    }
    pub fn from(b: Builder<M>) -> Self {
        Self {b, t: PhantomData}
    }
    pub fn eq<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" = {}", u.to_sql())))
    }
    pub fn neq<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" <> {}", u.to_sql())))
    }
    pub fn gt<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" > {}", u.to_sql())))
    }
    pub fn lt<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" < {}", u.to_sql())))
    }
    pub fn gte<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" >= {}", u.to_sql())))
    }
    pub fn lte<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_str(&format!(" <= {}", u.to_sql())))
    }
    pub fn is_in<U: ToSql + PartialEq<D::T>>(mut self, v: &Vec<U>) -> WhoseArg<M> {
        self.b.push(" IN(");
        let mut i = 0;
        for t in v {
            if i > 0 {
                self.b.push(&format!(", {}", t.to_sql()));
            } else {
                self.b.push(&format!(" {}", t.to_sql()));
            }
            i += 1;
        }
        WhoseArg::from(self.b.push_str(")"))
    }
    pub fn asc(self) -> OrderByArg<M> {
        OrderByArg {b: self.b.push_str(" ASC")}
    }
    pub fn desc(self) -> OrderByArg<M> {
        OrderByArg {b: self.b.push_str(" DESC")}
    }
    pub fn field(self, v: &Vec<D>) -> OrderByArg<M> {
        let mut b = Builder::new();
        b.push(&format!("CASE {}", self.b.val));
        let mut i = 0;
        for d in v {
            b.push(&format!(" WHEN {} THEN {i}", d.to_sql()));
            i += 1;
        }
        OrderByArg {b: b.push_str(" END")}
    }
}

pub struct Update<U: Record> {
    val: Builder<U>,
    count: u32,
}

impl<U: Record> Update<U> {
    pub fn new(database: &str) -> Self {
        let val = Builder::update(database);
        let count = 0;
        Self {val, count}
    }
    pub fn set<D: DataType>(mut self, name: &str, data: &D) -> Self {
        let s = data.to_sql();
        self.count += 1;
        let val = if self.count > 1 {
            self.val.push_str(&format!(", {} = {}", name, s))
        } else {
            self.val.push_str(&format!(" {} = {}", name, s))
        };
        Self {val, count: self.count}
    }
    pub fn pk<D: DataType + std::fmt::Display>(self, name: &str, id: D) -> Self {
        let val = self.val.push_str(&format!(" WHERE {} = {}", name, id));
        Self {val, count: self.count}
    }
    pub async fn update<B: BaseRequest>(self, req: &B) -> Result<()> {
        if !req.raw().valid_token() {
            return Err(invalid());
        }
        self.raw_update(req.raw().pool()).await
    }
    pub async fn raw_update(self, pool: &DbPool) -> Result<()> {
        let mut val = self.val.val();
        val.push_str(";\n");
        match sqlx::query(&val).execute(&pool.0).await {
            Ok(_) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
}

pub struct Insert<I: Record> {
    val: Builder<I>,
    n: usize,
}

impl<I: Record> Insert<I> {
    pub fn new(database: &str, columns: &[&str]) -> Self {
        let val = Builder::insert_into(database, columns);
        Self {val, n: 0}
    }
    pub fn value<D: DataType>(mut self, data: &D) -> Self {
        let s = data.to_sql();
        let val = if self.n > 0 {
            self.val.push_str(&format!(", {}", s))
        } else {
            self.val.push_str(&format!(" {}", s))
        };
        self.n += 1;
        Self {val, n: self.n}
    }
    pub async fn save<B: BaseRequest>(self, req: &B) -> Result<()> {
        if req.raw().valid_token() {
            self.raw_save(req.raw().pool()).await
        } else {
            Err(invalid())
        }
    }
    pub async fn raw_save(self, pool: &DbPool) -> Result<()> {
        let mut val = self.val.val();
        val.push_str(");\n");
        match sqlx::query(&val).execute(&pool.0).await {
            Ok(_) => {
                Ok(())
            },
            Err(e) => {
                Err(Box::new(e))
            },
        }
    }
}

pub async fn delete_from<B: BaseRequest>(table: &str, table_id: &str, id: BigInt, req: &B) -> Result<()> {
    if !req.raw().valid_token() {
        return Err(invalid());
    }
    let val = format!("DELETE FROM {} WHERE {} = {};\n", table, table_id, id);
   
    match sqlx::query(&val).execute(&req.raw().pool().0).await {
        Ok(_) => Ok(()),
        Err(e) => Err(Box::new(e)),
    }
}

pub struct Limit<M: Record> {
    val: Builder<M>,
}

impl<M: Record> Limit<M> {
    pub fn from(val: Builder<M>) -> Self {
        Self {val}
    }
    pub async fn query<B: BaseRequest>(self, req: &B) -> Result<Objects<M>> {
        self.raw_query(req.raw().pool()).await
    }
    async fn raw_query(self, pool: &DbPool) -> Result<Objects<M>> {
        let mut val = self.val.val();
        val.push_str(";\n");

        match sqlx::query(&val).fetch_all(&pool.0).await {
            Ok(rows) => {
                M::from(DbRowVec {rows})
            },
            Err(e) => {
                println!("{}", e);
                Err(invalid())
            },
        }
    }
}

pub struct LimitCount<M: Record> {
    val: Builder<M>,
}

impl<M: Record> LimitCount<M> {
    pub fn from(val: Builder<M>) -> Self {
        Self {val}
    }
    pub async fn query<B: BaseRequest>(self, req: &B) -> Result<Vec<u32>> {
        self.raw_query(req.raw().pool()).await
    }
    async fn raw_query(self, pool: &DbPool) -> Result<Vec<u32>> {
        use sqlx::Row;
        let mut val = self.val.val();
        val.push_str(";\n");
        let mut v = vec![];
        match sqlx::query(&val).fetch_all(&pool.0).await {
            Ok(rows) => {
                for row in rows {
                    v.push(row.try_get("count")?);
                }
                Ok(v)
            },
            Err(_) => Err(invalid()),
        }
    }
}
