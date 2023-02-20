#[cfg(feature = "sqlite")]
pub mod sqlite;

#[cfg(feature = "postgres")]
pub mod postgres;

use std::str;
use std::marker::PhantomData;

use async_trait::async_trait;

use crate::server::Settings;
use crate::records::{Record, DataType, Objects, ToSql};
use crate::web::{Result, BaseRequest, WebErrorKind};

#[cfg(feature = "sqlite")]
#[macro_export]
macro_rules! sql_bool {
    (true) => {
        1
    };
    (false) => {
        0
    };
}

#[cfg(feature = "postgres")]
#[macro_export]
macro_rules! sql_bool {
    (true) => {
        true
    };
    (false) => {
        false
    };
}

#[macro_export]
macro_rules! database {
    (sqlite) => {
        type Pool = anansi::db::sqlite::SqliteDbPool;
    };
    (postgres) => {
        type Pool = anansi::db::postgres::PgDbPool;
    };
}

pub trait DbRow {
    type RawRow;
    fn new(row: Self::RawRow) -> Self;
    fn try_bool(&self, index: &str) -> Result<bool>;
    fn try_i32(&self, index: &str) -> Result<i32>;
    fn try_i64(&self, index: &str) -> Result<i64>;
    fn try_count(&self) -> Result<i64>;
    fn try_option_string(&self, index: &str) -> Result<Option<String>>;
    fn try_string(&self, index: &str) -> Result<String>;
    fn try_date_time(&self, index: &str) -> Result<String>;
}

pub trait DbRowVec: IntoIterator<Item = Self::Row> {
    type Row: DbRow;
    type Rows;
    fn from(rows: Self::Rows) -> Self;
}

#[async_trait]
pub trait DbPool: Clone + Send + Sync + DbType {
    type SqlRow: DbRow;
    type SqlRowVec: DbRowVec;
    async fn new(settings: &Settings) -> Result<Self> where Self: Sized;
    async fn query(&self, val: &str) -> Result<Self::SqlRowVec>;
    async fn raw_fetch_one(&self, val: &str) -> Result<Self::SqlRow>;
    async fn raw_fetch_all(&self, val: &str) -> Result<Self::SqlRowVec>;
    async fn raw_execute(&self, val: &str) -> Result<()>;
    async fn test() -> Result<Self> where Self: Sized;
    fn to_stmt<R: Record>(val: Builder<R>) -> String;
    fn now() -> &'static str;
}

pub trait DbType {
    fn db_type(ty: &str) -> String;
}

#[macro_export]
macro_rules! try_sqlx {
    ($self:ident, $index:ident) => {
        {
            use sqlx::Row;
            match $self.row.try_get($index) {
                Ok(o) => Ok(o),
                Err(e) => Err(Box::new(e))
            }
        }
    }
}

#[macro_export]
macro_rules! try_sql {
    ($self:ident, $index:ident) => {
        {
            match $self.row.try_get($index) {
                Ok(o) => Ok(o),
                Err(e) => Err(Box::new(e))
            }
        }
    }
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
pub struct Builder<R: Record> {
    start: Vec<Clause<R>>,
    join: Vec<Join>,
    val: Vec<Clause<R>>
}

impl<R: Record> Clone for Builder<R> {
    fn clone(&self) -> Self {
        Self {start: self.start.clone(), join: self.join.clone(), val: self.val.clone()}
    }
}

#[derive(Debug)]
pub enum Clause<R: Record> {
    _M(PhantomData<R>),
    Count(String),
    Select(Vec<String>),
    From(String),
    Insert(String),
    Columns(Vec<String>),
    Delete(String),
    Update(String),
    And(Vec<Clause<R>>),
    Or(Vec<Clause<R>>),
    Comma,
    Whose,
    Column(String),
    OrderBy,
    Limit(u32),
    Offset(u32),
    Contains(String),
    Lower(Vec<Clause<R>>),
    LowerString(String),
    Like(String),
    LikeLower(String),
    Eq(String),
    Neq(String),
    Gt(String),
    Lt(String),
    Gte(String),
    Lte(String),
    In(Vec<String>),
    Asc,
    Desc,
    Case(Vec<Clause<R>>),
    When(String),
    Then(String),
    End,
    Set(String),
    Where(String),
    Value(String),
    Close,
    CloseInsert,
    Raw(String),
}
    
impl<R: Record> Clone for Clause<R> {
    fn clone(&self) -> Self {
        match self {
            Self::_M(p) => Self::_M(p.clone()),
            Self::Count(s) => Self::Count(s.clone()),
            Self::Select(v) => Self::Select(v.clone()),
            Self::From(s) => Self::From(s.clone()),
            Self::Insert(s) => Self::Insert(s.clone()),
            Self::Columns(v) => Self::Columns(v.clone()),
            Self::Delete(s) => Self::Delete(s.clone()),
            Self::Update(s) => Self::Update(s.clone()),
            Self::And(v) => Self::And(v.clone()),
            Self::Or(v) => Self::Or(v.clone()),
            Self::Comma => Self::Comma,
            Self::Whose => Self::Whose,
            Self::Column(s) => Self::Column(s.clone()),
            Self::OrderBy => Self::OrderBy,
            Self::Limit(u) => Self::Limit(*u),
            Self::Offset(u) => Self::Offset(*u),
            Self::Contains(s) => Self::Contains(s.clone()),
            Self::Lower(v) => Self::Lower(v.clone()),
            Self::LowerString(s) => Self::LowerString(s.clone()),
            Self::Like(s) => Self::Like(s.clone()),
            Self::LikeLower(s) => Self::LikeLower(s.clone()),
            Self::Eq(s) => Self::Eq(s.clone()),
            Self::Neq(s) => Self::Neq(s.clone()),
            Self::Gt(s) => Self::Gt(s.clone()),
            Self::Lt(s) => Self::Lt(s.clone()),
            Self::Gte(s) => Self::Gte(s.clone()),
            Self::Lte(s) => Self::Lte(s.clone()),
            Self::In(v) => Self::In(v.clone()),
            Self::Asc => Self::Asc,
            Self::Desc => Self::Desc,
            Self::Case(v) => Self::Case(v.clone()),
            Self::When(s) => Self::When(s.clone()),
            Self::Then(s) => Self::Then(s.clone()),
            Self::End => Self::End,
            Self::Set(s) => Self::Set(s.clone()),
            Self::Where(s) => Self::Where(s.clone()),
            Self::Value(s) => Self::Value(s.clone()),
            Self::Close => Self::Close,
            Self::CloseInsert => Self::CloseInsert,
            Self::Raw(s) => Self::Raw(s.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Join {
    pub t1: String,
    pub t2: String,
    pub c: String,
    pub d: String,
}

#[cfg(any(feature = "sqlite", feature = "postgres"))]
pub fn sql_stmt<R: Record>(builder: Builder<R>) -> String {
    let mut s = String::new();
    s.push_str(&mut clause_stmt(builder.start));
    for j in builder.join {
        s.push_str(&format!(" INNER JOIN {} ON {1}.{2} = {0}.{3}", j.t1, j.t2, j.c, j.d));
    }
    s.push_str(&mut clause_stmt(builder.val));
    s
}

#[cfg(any(feature = "sqlite", feature = "postgres"))]
pub fn clause_stmt<R: Record>(clauses: Vec<Clause<R>>) -> String {
    let mut stmt = String::new();
    let mut setn = 0;
    let mut valn = 0;
    for clause in clauses {
        let s = match clause {
            Clause::_M(_) => unimplemented!(),
            Clause::Count(s) => format!("SELECT COUNT(*) FROM {}", s),
            Clause::Select(columns) => {
                let mut select = format!("SELECT {}", columns[0]);
                for column in &columns[1..] {
                    select.push_str(&format!(", {}", column));
                }
                select
            }
            Clause::From(s) => format!(" FROM {s}"),
            Clause::Insert(s) => format!("INSERT INTO {s}"),
            Clause::Columns(v) => {
                let mut columns = format!("({}", v[0]);
                for col in &v[1..] {
                    columns.push_str(&format!(", {}", col));
                }
                columns.push_str(") VALUES (");
                columns
            }
            Clause::Delete(s) => format!("DELETE FROM {s}"),
            Clause::Update(s) => format!("UPDATE {} SET", s),
            Clause::And(v) => format!(" AND {}", clause_stmt(v)),
            Clause::Or(v) => format!(" OR {}", clause_stmt(v)),
            Clause::Comma => " , ".to_string(),
            Clause::Whose => " WHERE ".to_string(),
            Clause::Column(s) => format!("{}", s),
            Clause::OrderBy => " ORDER BY ".to_string(),
            Clause::Limit(u) => format!(" LIMIT {u}"),
            Clause::Offset(u) => format!(" OFFSET {u}"),
            Clause::Contains(s) => format!(" LIKE '%{s}%'"),
            Clause::Lower(v) => format!(" LOWER({})", clause_stmt(v)),
            Clause::LowerString(s) => format!(" LOWER({s})"),
            Clause::Like(s) => format!(" LIKE {s}"),
            Clause::LikeLower(s) => format!(" LIKE LOWER({s})"),
            Clause::Eq(s) => format!(" = {s}"),
            Clause::Neq(s) => format!(" <> {s}"),
            Clause::Gt(s) => format!(" > {s}"),
            Clause::Lt(s) => format!(" < {s}"),
            Clause::Gte(s) => format!(" >= {s}"),
            Clause::Lte(s) => format!(" <= {s}"),
            Clause::In(v) => {
                let mut i = format!(" IN ({}", v[0]);
                for s in &v[1..] {
                    i.push_str(&format!(", {s}"));
                }
                i.push(')');
                i
            }
            Clause::Asc => " ASC".to_string(),
            Clause::Desc => " DESC".to_string(),
            Clause::Case(v) => format!("CASE {}", clause_stmt(v)),
            Clause::When(s) => format!(" WHEN {s}"),
            Clause::Then(s) => format!(" THEN {s}"),
            Clause::End => " END".to_string(),
            Clause::Set(s) => {
                setn += 1;
                if setn > 1 {
                    format!(", \"{s}\"")
                } else {
                    format!(" \"{s}\"")
                }
            }
            Clause::Where(s) => format!(" WHERE {}", s),
            Clause::Value(s) => {
                valn += 1;
                if valn > 1 {
                    format!(", {s}")
                } else {
                    format!(" {s}")
                }
            }
            Clause::Close => ";\n".to_string(),
            Clause::CloseInsert => ");\n".to_string(),
            Clause::Raw(s) => s,
        };
        stmt.push_str(&s);
    }
    stmt
}

impl<B: Record> Builder<B> {
    pub fn new() -> Self {
        Self {start: vec![], join: vec![], val: vec![]}
    }
    fn from(start: Vec<Clause<B>>) -> Self {
        Self {start, join: vec![], val: vec![]}
    }
    pub fn count(database: &str) -> Self {
        let start = Clause::Count(database.to_string());
        Self::from(vec![start])
    }
    pub fn select(columns: &[&str], database: &str) -> Self {
        let mut cols = vec![];
        for column in columns {
            cols.push(format!("{}.{}", database, column));
        }
        let start = vec![Clause::Select(cols), Clause::From(database.to_string())];
        Self::from(start)
    }
    pub fn insert_into(database: &str, columns: &[&str])  -> Self {
        let mut start = vec![Clause::Insert(database.to_string())];
        let mut cols = vec![];
        for column in columns {
            cols.push(column.to_string());
        }
        start.push(Clause::Columns(cols));
        Self::from(start)
    }
    pub fn delete(database: &str)  -> Self {
        let start = vec![Clause::Delete(database.to_string())];
        Self::from(start)
    }
    pub fn update(database: &str)  -> Self {
        let start = vec![Clause::Update(database.to_string())];
        Self::from(start)
    }
    pub fn push(&mut self, s: Clause<B>) {
        self.val.push(s);
    }
    pub fn push_val(mut self, s: Clause<B>) -> Self {
        self.val.push(s);
        self
    }
    pub fn append(mut self, mut other: Self) -> Self {
        self.val.append(&mut other.val);
        self.join.append(&mut other.join);
        self
    }
    pub fn comma(mut self) -> Self {
        self.val.push(Clause::Comma);
        self
    }
    pub fn whose(mut self) -> Self {
        self.val.push(Clause::Whose);
        self
    }
    pub fn column(mut self, c: &str) -> Self {
        self.val.push(Clause::Column(c.to_string()));
        self
    }
    pub fn inner_join(mut self, t1: &str, t2: &str, c: &str, d: &str) -> Self {
        let s = Join {t1: t1.to_string(), t2: t2.to_string(), c: c.to_string(), d: d.to_string()};
        self.join.push(s);
        self
    }
    pub fn order_by(mut self) -> Self {
        self.val.push(Clause::OrderBy);
        self
    }
    pub fn limit(mut self, n: u32) -> Self {
        self.val.push(Clause::Limit(n));
        self
    }
    pub fn offset(mut self, n: u32) -> Self {
        self.val.push(Clause::Offset(n));
        self
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
    pub async fn raw_get<D: DbPool>(self, pool: &D) -> Result<i64> {
        self.stmt.raw_get_count(pool).await
    }
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<i64> {
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
            return Err(WebErrorKind::BadToken.to_box());
        }
        let val = B::SqlPool::to_stmt(self.stmt.val.push_val(Clause::Close));
       
        req.raw().pool().raw_execute(&val).await
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
    pub async fn raw_get<D: DbPool>(self, pool: &D) -> Result<M> {
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
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<i64> {
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
    pub async fn get<B: BaseRequest>(self, req: &B) -> Result<i64> {
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
        OrderBy::from(self.val.push_val(arg.b.val[0].clone()))
    }
    fn order_by_count(self, arg: OrderByArg<S>) -> OrderByCount<S> {
        OrderByCount::from(self.val.push_val(arg.b.val[0].clone()))
    }
    fn group_by_count<D: DataType>(self, arg: Column<S, D>) -> GroupByCount<S> {
        GroupByCount::from(self.val.push_val(arg.b.val[0].clone()))
    }
    fn and(mut self, mut val: Builder<S>) -> Self {
        self.val.val.push(Clause::And(val.val));
        self.val.join.append(&mut val.join);
        self
    }
    fn or(mut self, mut val: Builder<S>) -> Self {
        self.val.val.push(Clause::Or(val.val));
        self.val.join.append(&mut val.join);
        self
    }
    pub async fn raw_get_count<'r, D: DbPool>(self, pool: &D) -> Result<i64> {
        let val = D::to_stmt(self.val.push_val(Clause::Close));

        match pool.raw_fetch_one(&val).await {
            Ok(row) => Ok(row.try_count()?),
            Err(e) => Err(e),
        }
    }
    async fn get_count<B: BaseRequest>(self, req: &B) -> Result<i64> {
        self.raw_get_count(req.raw().pool()).await
    }
    async fn raw_get<D: DbPool>(self, pool: &D) -> Result<S> {
        let val = D::to_stmt(self.val.push_val(Clause::Close));

        match pool.raw_fetch_one(&val).await {
            Ok(row) => {
                S::get(row)
            }
            Err(e) => {
                Err(e)
            }
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
        WhoseArg::from(self.b.push_val(Clause::Contains(percent_escape(&u.to_sql()))))
    }
    pub fn icontains<'a, U: ToSql + std::fmt::Display + PartialEq<&'a str>>(mut self, u: U) -> WhoseArg<M> {
        self.b.val.push(Clause::Lower(self.b.val.clone()));
        self.b.val.push(Clause::LikeLower(format!("'%{}%'", percent_escape(&u.to_string()))));
        WhoseArg::from(self.b)
    }
    pub fn iexact<'a, U: ToSql + PartialEq<&'a str>>(mut self, u: U) -> WhoseArg<M> {
        self.b.val.push(Clause::Lower(self.b.val.clone()));
        self.b.val.push(Clause::LowerString(u.to_sql()));
        WhoseArg::from(self.b)
    }
    pub fn starts_with<'a, U: ToSql + PartialEq<&'a str>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Like(format!("\"{}%\"", percent_escape(&u.to_sql())))))
    }
    pub fn ends_with<'a, U: ToSql + PartialEq<&'a str>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Like(format!("\"%{}\"", percent_escape(&u.to_sql())))))
    }
}

impl<M: Record, D: DataType> Column<M, D> {
    pub fn new(s: &str) -> Self {
        let b = Builder::new().push_val(Clause::Column(s.to_string()));
        Self {b, t: PhantomData}
    }
    pub fn from(b: Builder<M>) -> Self {
        Self {b, t: PhantomData}
    }
    pub fn eq<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Eq(u.to_sql())))
    }
    pub fn neq<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Neq(u.to_sql())))
    }
    pub fn gt<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Gt(u.to_sql())))
    }
    pub fn lt<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Lt(u.to_sql())))
    }
    pub fn gte<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Gte(u.to_sql())))
    }
    pub fn lte<U: ToSql + PartialEq<D::T>>(self, u: U) -> WhoseArg<M> {
        WhoseArg::from(self.b.push_val(Clause::Lte(u.to_sql())))
    }
    pub fn is_in<U: ToSql + PartialEq<D::T>>(self, v: &Vec<U>) -> WhoseArg<M> {
        let mut tv = vec![];
        for t in v {
            tv.push(t.to_sql());
        }
        WhoseArg::from(self.b.push_val(Clause::In(tv)))
    }
    pub fn asc(self) -> OrderByArg<M> {
        OrderByArg {b: self.b.push_val(Clause::Asc)}
    }
    pub fn desc(self) -> OrderByArg<M> {
        OrderByArg {b: self.b.push_val(Clause::Desc)}
    }
    pub fn field(self, v: &Vec<D>) -> OrderByArg<M> {
        let mut b = Builder::new();
        b.push(Clause::Case(self.b.val.clone()));
        let mut i = 0;
        for d in v {
            b.push(Clause::When(d.to_sql()));
            b.push(Clause::Then(i.to_string()));
            i += 1;
        }
        OrderByArg {b: b.push_val(Clause::End)}
    }
}

pub struct Update<U: Record> {
    val: Builder<U>,
}

impl<U: Record> Update<U> {
    pub fn new(database: &str) -> Self {
        let val = Builder::update(database);
        Self {val}
    }
    pub fn set<D: DataType>(mut self, name: &str, data: &D) -> Self {
        let s = data.to_sql();
        self.val.push(Clause::Set(name.to_string()));
        let val = self.val.push_val(Clause::Eq(s));
        Self {val}
    }
    pub fn bulk_set(&mut self, name: &str, pk_name: &str) {
        self.val.push(Clause::Set(name.to_string()));
        self.val.push(Clause::Eq(format!("CASE {}", pk_name)));
    }
    pub fn where_pk(mut self, s: String) -> Self {
        self.val.push(anansi::db::Clause::Where(s));
        self
    }
    pub fn when(&mut self, s: String) {
        self.val.push(anansi::db::Clause::When(s));
    }
    pub fn eq(&mut self, s: String) {
        self.val.push(anansi::db::Clause::Eq(s));
    }
    pub fn then(&mut self, s: String) {
        self.val.push(anansi::db::Clause::Then(s));
    }
    pub fn end(&mut self) {
        self.val.push(anansi::db::Clause::End);
    }
    pub fn is_in(self, v: &Vec<U>) -> Self {
        let mut tv = vec![];
        for t in v {
            tv.push(t.pk().to_sql());
        }
        let val = self.val.push_val(Clause::In(tv));
        Self {val}
    }
    pub fn pk<D: DataType + std::fmt::Display>(mut self, name: &str, id: D) -> Self {
        self.val.push(Clause::Where(name.to_string()));
        let val = self.val.push_val(Clause::Eq(id.to_string()));
        Self {val}
    }
    pub async fn update<B: BaseRequest>(self, req: &B) -> Result<()> {
        if !req.raw().valid_token() {
            return Err(WebErrorKind::BadToken.to_box());
        }
        self.raw_update(req.raw().pool()).await
    }
    pub async fn raw_update<D: DbPool>(self, pool: &D) -> Result<()> {
        let val = D::to_stmt(self.val.push_val(Clause::Close));
        pool.raw_execute(&val).await
    }
}

pub struct Insert<I: Record> {
    val: Builder<I>,
}

impl<I: Record> Insert<I> {
    pub fn new(database: &str, columns: &[&str]) -> Self {
        let val = Builder::insert_into(database, columns);
        Self {val}
    }
    pub fn value<D: DataType>(self, data: &D) -> Self {
        let s = data.to_sql();
        let val = self.val.push_val(Clause::Value(s));
        Self {val}
    }
    pub async fn save<B: BaseRequest>(self, req: &B) -> Result<()> {
        if req.raw().valid_token() {
            self.raw_save(req.raw().pool()).await
        } else {
            Err(WebErrorKind::BadToken.to_box())
        }
    }
    pub async fn raw_save<D: DbPool>(self, pool: &D) -> Result<()> {
        let val = D::to_stmt(self.val.push_val(Clause::CloseInsert));

        match pool.raw_execute(&val).await {
            Ok(_) => {
                Ok(())
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}

pub async fn delete_from<B: BaseRequest, D: DataType + std::fmt::Display>(table: &str, table_id: &str, id: D, req: &B) -> Result<()> {
    if !req.raw().valid_token() {
        return Err(WebErrorKind::BadToken.to_box());
    }
    let val = format!("DELETE FROM {} WHERE {} = {};\n", table, table_id, id);
   
    req.raw().pool().raw_execute(&val).await
}

pub struct Offset<M: Record> {
    val: Builder<M>,
}

impl<M: Record> Offset<M> {
    pub fn from(val: Builder<M>) -> Self {
        Self {val}
    }
    pub async fn query<B: BaseRequest>(self, req: &B) -> Result<Objects<M>> {
        self.raw_query(req.raw().pool()).await
    }
    async fn raw_query<D: DbPool>(self, pool: &D) -> Result<Objects<M>> {
        let val = D::to_stmt(self.val.push_val(Clause::Close));

        match pool.raw_fetch_all(&val).await {
            Ok(rows) => {
                M::from(rows)
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}

pub struct Limit<M: Record> {
    val: Builder<M>,
}

impl<M: Record> Limit<M> {
    pub fn from(val: Builder<M>) -> Self {
        Self {val}
    }
    pub fn offset(self, n: u32) -> Offset<M> {
        Offset::from(self.val.offset(n))
    }
    pub async fn query<B: BaseRequest>(self, req: &B) -> Result<Objects<M>> {
        self.raw_query(req.raw().pool()).await
    }
    pub async fn raw_query<D: DbPool>(self, pool: &D) -> Result<Objects<M>> {
        let val = D::to_stmt(self.val.push_val(Clause::Close));

        match pool.raw_fetch_all(&val).await {
            Ok(rows) => {
                M::from(rows)
            }
            Err(e) => {
                Err(e)
            }
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
    pub async fn query<B: BaseRequest>(self, req: &B) -> Result<Vec<i64>> where <<<B as BaseRequest>::SqlPool as DbPool>::SqlRowVec as IntoIterator>::Item: DbRow {
        self.raw_query(req.raw().pool()).await
    }
    async fn raw_query<D: DbPool>(self, pool: &D) -> Result<Vec<i64>> where <D::SqlRowVec as IntoIterator>::Item: DbRow {
        let val = D::to_stmt(self.val.push_val(Clause::Close));
        let mut v = vec![];
        match pool.raw_fetch_all(&val).await {
            Ok(rows) => {
                for row in rows {
                    v.push(row.try_count()?);
                }
                Ok(v)
            }
            Err(e) => Err(e),
        }
    }
}
