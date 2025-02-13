use sqlite::{Connection, Error, State};

use crate::models::cmus_status;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn get_raw_cmus_events_from_db(db: &Connection)
-> Result<Vec<RawCmusEvent>, String> {
  get_events(db).map_err(|ω| ω.to_string())
}

fn get_events(db: &Connection) -> Result<Vec<RawCmusEvent>, Error> {
  let query = "
    SELECT time,
           status,
           artist,
           title,
           album,
           album_artist,
           genre,
           duration,
           date
      FROM raw_cmus_events
     ORDER BY time ASC
    ";
  let mut statement = db.prepare(query)?;
  let mut events = Vec::new();
  while let State::Row = statement.next()? {
    let event = RawCmusEvent {
      time: statement.read(0)?,
      status: statement.read(1).and_then(cmus_status::from_sql_enum)?,
      artist: statement.read(2)?,
      title: statement.read(3)?,
      album: statement.read(4)?,
      album_artist: statement.read(5)?,
      genre: statement.read(6)?,
      duration: statement.read(7)?,
      date: statement.read(8)?
    };
    events.push(event);
  }
  Ok(events)
}
