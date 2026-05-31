use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int, sql_string};
use crate::models::er::Er;

pub fn write(db: &Connection, artist_id: i64, country: String) 
-> Result<(), Er> {
  let query = "
    UPDATE artists
       SET country = (SELECT id FROM countries WHERE abbreviation = ?)
     WHERE id      = ?
  ";
  let mut statement = db.prepare(query)?;
  sql_string(&mut statement, 1, country)?;
  sql_int(&mut statement, 2, artist_id)?;
  Ok(sql_execute_void(&mut statement)?)
}
