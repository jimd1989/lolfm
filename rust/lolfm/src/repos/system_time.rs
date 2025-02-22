use std::time::{SystemTime, UNIX_EPOCH};
use crate::models::er::Er;

pub fn get() -> Result<i64, Er> {
  let t = SystemTime::now().duration_since(UNIX_EPOCH)?;
  Ok(t.as_millis() as i64)
}
