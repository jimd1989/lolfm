use sqlite::{Connection, Error, State};

use crate::helpers::sql_helpers::{sql_int, sql_nullable_string};
use crate::models::cmus_status;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn write_raw_cmus_event_to_db(db: &Connection, event: RawCmusEvent) 
-> Result<(), String> {
  match write_event(db, event) {
    Ok(State::Done) => Ok(()),
    Err(ω)          => Err(ω.to_string()),
    _               => Err("error writing raw cmus event".to_string()),
  }
}

fn write_event(db: &Connection, event: RawCmusEvent) -> Result<State, Error> {
  let query = "
    INSERT INTO raw_cmus_events (
                  status,
                  artist,
                  title,
                  album,
                  album_artist,
                  genre,
                  duration,
                  date
                )
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
  let mut statement = db.prepare(query)?;
  sql_int(&mut statement, 1, cmus_status::to_sql_enum(event.status))
    .and_then(|_| sql_nullable_string(&mut statement, 2, event.artist))
    .and_then(|_| sql_nullable_string(&mut statement, 3, event.title))
    .and_then(|_| sql_nullable_string(&mut statement, 4, event.album))
    .and_then(|_| sql_nullable_string(&mut statement, 5, event.album_artist))
    .and_then(|_| sql_nullable_string(&mut statement, 6, event.genre))
    .and_then(|_| sql_int(&mut statement, 7, event.duration))
    .and_then(|_| sql_int(&mut statement, 8, event.date))?;
  statement.next()
}
