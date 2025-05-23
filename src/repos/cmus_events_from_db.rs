use sqlite::{Connection, State, Statement};
use std::iter;

use crate::helpers::sql_helpers::sql_int;
use crate::models::cmus_status::CmusStatus;
use crate::models::er::Er;
use crate::models::cmus_event::CmusEvent;
use crate::models::timestamp::Milliseconds;

pub fn get<'a>(db: &'a Connection, time: Milliseconds)
-> Result<impl Iterator<Item = Result<CmusEvent, Er>> + 'a, Er> {
  let query = "
    SELECT time_milliseconds,
           status,
           artist,
           title,
           album,
           album_artist,
           genre,
           disc_number,
           track_number,
           duration,
           date
      FROM raw_cmus_events
     WHERE time_milliseconds <= ?
     ORDER BY time_milliseconds ASC
    ";
  let mut statement = db.prepare(query)?;
  sql_int(&mut statement, 1, time.to_i64())?;
  Ok(iter::from_fn(move || {
    match statement.next() {
      Ok(State::Row)  => Some(to_event(&mut statement)),
      Ok(State::Done) => None,
      Err(ω)          => Some(Err(Er::from(ω)))
    }
  }))
}

fn to_event(statement: &mut Statement) -> Result<CmusEvent, Er> {
  let raw_status = statement.read(1)?;
  let status = CmusStatus::from_sql_enum(raw_status)?;
  let raw_time: i64 = statement.read(0)?;
  let event = CmusEvent {
    time:         Milliseconds::from_i64(raw_time),
    status:       status,
    artist:       statement.read(2)?,
    title:        statement.read(3)?,
    album:        statement.read(4)?,
    album_artist: statement.read(5)?,
    genre:        statement.read(6)?,
    disc_number:  statement.read(7)?,
    track_number: statement.read(8)?,
    duration:     statement.read(9)?,
    date:         statement.read(10)?
  };
  Ok(event)
}
