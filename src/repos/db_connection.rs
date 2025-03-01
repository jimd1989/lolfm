use sqlite::Connection;

use crate::models::er::Er;

pub fn get(db_path: &String) -> Result<Connection, Er> {
  Ok(sqlite::open(db_path)?)
}
