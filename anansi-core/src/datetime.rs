use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};
use std::result;
use std::error::Error;

use crate::web::Result;
use crate::records::{DataType, RecordField, ToSql};
use sqlx::{Decode, Database, database::HasValueRef};
use serde::{Serialize, Deserialize};
use chrono::{Datelike, Timelike};
use chrono::naive::{NaiveDate, NaiveTime, NaiveDateTime};

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Date {
    date: NaiveDate,
}

impl Date {
    pub fn year(&self) -> i32 {
        self.date.year()
    }
    pub fn month(&self) -> u32 {
        self.date.month()
    }
    pub fn day(&self) -> u32 {
        self.date.day()
    }
    pub fn from(s: &str) -> Result<Self> {
        match NaiveDate::parse_from_str(s, "%Y-%m-%d") {
            Ok(date) => Ok(Self {date}),
            Err(e) => Err(Box::new(e)),
        }
    }
}

impl fmt::Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.date)
    }
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Time {
    time: NaiveTime
}

impl Time {
    pub fn hour(&self) -> u32 {
        self.time.hour()
    }
    pub fn minute(&self) -> u32 {
        self.time.minute()
    }
    pub fn second(&self) -> u32 {
        self.time.second()
    }
    pub fn from(s: &str) -> Result<Self> {
        match NaiveTime::parse_from_str(s, "%H:%M:%S") {
            Ok(time) => Ok(Self {time}),
            Err(e) => Err(Box::new(e)),
        }
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.time)
    }
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct DateTime {
    datetime: NaiveDateTime,
}

impl DateTime {
    pub fn now() -> Self {
        let s = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
        Self::from_secs(s as i64).unwrap()
    }
    pub fn after(secs: i64) -> Option<Self> {
        let s = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as i64 + secs;
        Self::from_secs(s)
    }
    pub fn to_gmt(&self) -> String {
        self.datetime.format("%a, %C %b %Y %H:%M:%S GMT").to_string()
    }
    pub fn year(&self) -> i32 {
        self.datetime.year()
    }
    pub fn month(&self) -> u32 {
        self.datetime.month()
    }
    pub fn day(&self) -> u32 {
        self.datetime.day()
    }
    pub fn hour(&self) -> u32 {
        self.datetime.hour()
    }
    pub fn minute(&self) -> u32 {
        self.datetime.minute()
    }
    pub fn second(&self) -> u32 {
        self.datetime.second()
    }
    fn from_secs(s: i64) -> Option<Self> {
        if let Some(datetime) = NaiveDateTime::from_timestamp_opt(s, 0) {
            Some(Self {datetime})
        } else {
            None
        }
    }
    pub fn field() -> RecordField {
        RecordField::new("datetime".to_string())
    }
}

impl DataType for DateTime {
    type T = String;
    
    fn from_val(s: String) -> Result<Self> {
        match NaiveDateTime::parse_from_str(&s, "%Y-%m-%d %H:%M:%S") {
            Ok(datetime) => Ok(Self {datetime}),
            Err(e) => Err(Box::new(e)),
        }
    }
}

impl ToSql for DateTime {
    fn to_sql(&self) -> String {
        format!("{}", self.datetime)
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.datetime)
    }
}

impl<'r, DB: Database> Decode<'r, DB> for DateTime
where String: Decode<'r, DB> {
    fn decode(value: <DB as HasValueRef<'r>>::ValueRef) -> result::Result<DateTime, Box<dyn Error + 'static + Send + Sync>> {
        let value = <String as Decode<DB>>::decode(value)?;
        Ok(Self::from_val(value).unwrap())
    }
}
