use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};
use std::result;
use std::error::Error;

use crate::db::{invalid, Db, DbTypeInfo};
use crate::web::Result;
use crate::models::{DataType, ModelField};
use sqlx::{Type, Decode, Database, database::HasValueRef};

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct Date {
    year: u16,
    month: u8,
    day: u8,
}

impl Date {
    pub fn year(&self) -> u16 {
        self.year
    }
    pub fn month(&self) -> u8 {
        self.month
    }
    pub fn day(&self) -> u8 {
        self.day
    }
    pub fn from(s: &str) -> Result<Self> {
        let v: Vec<&str> = s.split('-').collect();
        if v.len() != 3 {
            Err(invalid())
        } else {
            let year: u16 = v[0].parse()?;
            if year < 100 || year > 9999 {
                return Err(invalid());
            }
            let month: u8 = v[1].parse()?;
            if month > 12 {
                return Err(invalid());
            }
            let day: u8 = v[2].parse()?;
            let last = if month != 2 {
                if month % 2 == 0 {
                    31
                } else {
                    30
                }
            } else {
                if year % 4 != 0 {
                    28
                } else if year % 100 != 0 {
                    29
                } else if year % 400 != 0 {
                    28
                } else {
                    29
                }
            };
            if day > last {
                return Err(invalid());
            }
            Ok(Self {year, month, day})
        }
    }
    fn day_of_week(&self) -> &str {
        let mut day = self.day as usize;
        let year = self.year as usize;
        let months = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
        let week = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
        day += (year - 1971) * 365 + (year - 1969) / 4 + months[self.month as usize - 1];
        if self.year % 4 == 0 && self.month > 2 {
            day += 1;
        }
        week[day % 7]
    }
}

impl fmt::Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#04}-{:#02}-{:#02}", self.year, self.month, self.day)
    }
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct Time {
    hour: u8,
    minute: u8,
    second: u8,
}

impl Time {
    pub fn hour(&self) -> u8 {
        self.hour
    }
    pub fn minute(&self) -> u8 {
        self.minute
    }
    pub fn second(&self) -> u8 {
        self.second
    }
    pub fn from(s: &str) -> Result<Self> {
        let v: Vec<&str> = s.split(':').collect();
        if v.len() != 3 {
            Err(invalid())
        } else {
            let hour: u8 = v[0].parse()?;
            if hour > 23 {
                return Err(invalid());
            }
            let minute: u8 = v[1].parse()?;
            if minute > 59 {
                return Err(invalid());
            }
            let second: u8 = v[2].parse()?;
            if second > 59 {
                return Err(invalid());
            }
            Ok(Self {hour, minute, second})
        }
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#02}:{:#02}:{:#02}", self.hour, self.minute, self.second)
    }
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct DateTime {
    pub date: Date,
    pub time: Time,
}

impl DateTime {
    pub fn now() -> Self {
        let s = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
        Self::from_secs(s)
    }
    pub fn after(secs: u64) -> Self {
        let s = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() + secs;
        Self::from_secs(s)
    }
    pub fn to_gmt(&self) -> String {
        let months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
        format!("{}, {} {} {} {} GMT", self.date.day_of_week(), self.date.day, months[self.date.month as usize - 1], self.date.year, self.time)
    }
    fn from_secs(mut s: u64) -> Self {
        let mut days = 365;
        let mut year = 0;
        while s > days * 86400 {
            days = if year % 4 != 0 {
                365
            } else if year % 100 != 0 {
                366
            } else if year % 400 != 0 {
                365
            } else {
                366
            };
            year += 1;
            s -= days * 86400;
        }
        let mut yday = s/86400;
        s -= yday*86400;
        year += 1970;
        let last = if year % 4 != 0 {
            28
        } else if year % 100 != 0 {
            29
        } else if year % 400 != 0 {
            28
        } else {
            29
        };
        let days = vec![31, last, 31, 30, 31, 30, 31, 30, 31, 30, 31];
        let mut month = 1;
        for day in days {
            if yday > day {
                month += 1;
                yday -= day;
            } else {
                break;
            }
        }
        let hour = s/3600;
        s -= hour*3600;
        let minute = s/60;
        s -= minute*60;
        Self {date: Date {year, month, day: yday as u8 + 1}, time: Time {hour: hour as u8, minute: minute as u8, second: s as u8}}
    }
    pub fn field() -> ModelField {
        ModelField::new("datetime".to_string())
    }
}

impl DataType for DateTime {
    type T = String;
    
    fn from_val(s: String) -> Result<Self> {
        let v: Vec<&str> = s.split(' ').collect();
        if v.len() != 2 {
            Err(invalid())
        } else {
            let date = Date::from(v[0])?;
            let time = Time::from(v[1])?;
            Ok(Self {date, time})
        }
    }
    fn to_sql(&self) -> String {
        format!("'{} {}'", self.date, self.time)
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.date, self.time)
    }
}
impl Type<Db> for DateTime {
    fn type_info() -> DbTypeInfo {
        <String as Type<Db>>::type_info()
    }
}

impl<'r, DB: Database> Decode<'r, DB> for DateTime
where String: Decode<'r, DB> {
    fn decode(value: <DB as HasValueRef<'r>>::ValueRef) -> result::Result<DateTime, Box<dyn Error + 'static + Send + Sync>> {
        let value = <String as Decode<DB>>::decode(value)?;
        Ok(Self::from_val(value).unwrap())
    }
}
