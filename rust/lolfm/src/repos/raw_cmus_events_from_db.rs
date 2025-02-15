use sqlite::{Connection, State};

use crate::models::cmus_status::CmusStatus;
use crate::models::er::Er;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn get_raw_cmus_events_from_db(db: &Connection)
-> Result<Vec<RawCmusEvent>, String> {
  get_events(db).map_err(|ω| ω.to_string())
}

fn get_events(db: &Connection) -> Result<Vec<RawCmusEvent>, Er> {
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
     ORDER BY time_milliseconds ASC
    ";
  let mut statement = db.prepare(query)?;
  let mut events = Vec::new();
  while let State::Row = statement.next()? {
    let raw_status = statement.read(1)?;
    let status = CmusStatus::from_sql_enum(raw_status)?;
    let event = RawCmusEvent {
      time_milliseconds: statement.read(0)?,
      status:            status,
      artist:            statement.read(2)?,
      title:             statement.read(3)?,
      album:             statement.read(4)?,
      album_artist:      statement.read(5)?,
      genre:             statement.read(6)?,
      disc_number:       statement.read(7)?,
      track_number:      statement.read(8)?,
      duration:          statement.read(9)?,
      date:              statement.read(10)?
    };
    events.push(event);
  }
  Ok(events)
}
