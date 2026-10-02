use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_string};
use crate::models::artist::Artist;
use crate::models::er::Er;

pub fn write(db: &Connection, ω: impl Iterator<Item = Result<Artist, Er>>) 
-> Result<(), Er> {
  let query = "
    UPDATE artists
       SET country = (
           SELECT id
             FROM countries
            WHERE abbreviation = ?
           )
     WHERE name = ?
  ";
  let mut statement = db.prepare(query)?;
  for res in ω {
    let α = res?;
    sql_string(&mut statement, 1, α.country_abbreviation)?;
    sql_string(&mut statement, 2, α.name)?;
    sql_execute_void(&mut statement)?;
    statement.reset()?;
  }
  Ok(())
}
