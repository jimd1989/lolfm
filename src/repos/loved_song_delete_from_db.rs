use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int};
use crate::models::er::Er;

pub fn run(db: &Connection, id: i64) -> Result<(), Er> {
  let query = "
    DELETE FROM loved
     WHERE song = ?
  ";
  let mut statement = db.prepare(query)?;
  sql_int(&mut statement, 1, id)?;
  Ok(sql_execute_void(&mut statement)?)
}
