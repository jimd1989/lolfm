use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int};
use crate::models::er::Er;
use crate::models::timestamp::Milliseconds;

pub fn write(db: &Connection, time: Milliseconds, id: i64) -> Result<(), Er> {
  let query = "
    INSERT INTO loved(date, song)
    SELECT ?, songs.id
      FROM songs
     WHERE songs.id = ?
        ON CONFLICT DO NOTHING
  ";
  let mut statement = db.prepare(query)?;
  sql_int(&mut statement, 1, time.to_seconds().to_i64())?;
  sql_int(&mut statement, 2, id)?;
  Ok(sql_execute_void(&mut statement)?)
}
