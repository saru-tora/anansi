use std::collections::HashMap;

use crate::web::Result;
use crate::records::{DateTime, BigInt, DataType, RecordErrorKind};

const ENCODE: &[u8] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_";

thread_local! {
    static DECODE: HashMap<u8, i64> = {
        let mut hm = HashMap::new();
        let mut count = 0;
        for b in ENCODE {
            hm.insert(*b, count);
            count += 1;
        }
        hm
    }
}

pub fn encode(bi: BigInt) -> String {
    let mut n = bi.into();
    let mut v = vec![];
    loop {
        v.push(ENCODE[(n%64) as usize]);
        n /= 64;
        if n == 0 {
            break;
        }
    }
    v.reverse();
    String::from_utf8(v).unwrap()
}

pub fn decode(s: &str) -> Result<BigInt> {
    let mut n = 0;
    let mut m = 1;
    let mut c = 0;
    for b in s.as_bytes().iter().rev() {
        DECODE.with(|d| match d.get(b) {
            Some(o) => {
                n += o * m;
                Ok(())
            }
            None => return Err(RecordErrorKind::BadIntDecode.to_box()),
        })?;
        c += 1;
        if c < s.len() {
            m *= 64;
        }
    }
    Ok(BigInt::from_val(n)?)
}

pub fn ago(dt: DateTime) -> String {
    let now = DateTime::now();
    let year_diff = now.date.year() as i32 - dt.date.year() as i32;
    let month_diff = now.date.month() as i32 - dt.date.month() as i32;
    let day_diff = now.date.day() as i32 - dt.date.day() as i32;
    if year_diff > 0 && month_diff >= 0 && day_diff >= 0 {
        plural(year_diff, "year")
    } else if month_diff > 1 && day_diff >= 0 {
        plural(month_diff, "month")
    } else if day_diff > 0 {
        plural(day_diff, "day")
    } else {
        let hour_diff = now.time.hour() as i32 - dt.time.hour() as i32;
        let minute_diff = now.time.minute() as i32 - dt.time.minute() as i32;
        if hour_diff > 1 {
            plural(hour_diff, "hour")
        } else if hour_diff == 1 {
            if minute_diff >= 0 {
                plural(hour_diff, "hour")
            } else {
                plural(hour_diff * 60 + minute_diff, "minute")
            }
        } else if minute_diff > 0 {
            plural(minute_diff, "minute")
        } else {
            "just now".to_string()
        }
    }
}

fn plural(n: i32, unit: &str) -> String {
    if n == 1 {
        if unit == "hour" {
            "an hour ago".to_string()
        } else {
            format!("a {} ago", unit)
        }
    } else {
        format!("{} {}s ago", n, unit)
    }
}

pub fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    c.next().unwrap().to_uppercase().collect::<String>() + c.as_str()
}
