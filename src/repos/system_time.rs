use std::time::{SystemTime, UNIX_EPOCH};
use crate::models::er::Er;
use crate::models::timestamp::Milliseconds;

pub fn get() -> Result<Milliseconds, Er> {
  let t = SystemTime::now().duration_since(UNIX_EPOCH)?;
  Ok(Milliseconds::from_i64(t.as_millis() as i64))
}
