use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int};
use crate::models::er::Er;

pub fn run(db: &Connection, timestamp: i64) -> Result<(), Er> {
  let query = "
    DELETE FROM raw_cmus_events
     WHERE time_milliseconds < ?
     ";
  let mut statement = db.prepare(query)?;
  sql_int(&mut statement, 1, timestamp)?;
  Ok(sql_execute_void(&mut statement)?)
}
