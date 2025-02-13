use sqlite::{Connection, Error, State};

use crate::helpers::sql_helpers::sql_int;

pub fn delete_raw_cmus_events_from_db(db: &Connection, timestamp: i64) 
-> Result<(), String> {
  match delete_events(db, timestamp) {
    Ok(State::Done) => Ok(()),
    Err(ω)          => Err(ω.to_string()),
    _               => Err("error deleting raw cmus events".to_string()),
  }
}

fn delete_events(db: &Connection, timestamp: i64) -> Result<State, Error> {
  let query = "
    DELETE FROM raw_cmus_events
     WHERE time <= ?
     ";
  let mut statement = db.prepare(query)?;
  sql_int(&mut statement, 1, timestamp)?;
  statement.next()
}
