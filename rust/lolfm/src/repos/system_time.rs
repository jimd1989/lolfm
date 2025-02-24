use std::time::{SystemTime, UNIX_EPOCH};
use crate::models::er::Er;
use crate::models::timestamp::Timestamp;

pub fn get() -> Result<Timestamp, Er> {
  let t = SystemTime::now().duration_since(UNIX_EPOCH)?;
  Ok(Timestamp::from_milliseconds(t.as_millis() as i64))
}
