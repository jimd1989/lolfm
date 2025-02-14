use sqlite::{Connection, Error, State};

use crate::helpers::sql_helpers::{
  sql_int, sql_nullable_int, sql_nullable_string
};
use crate::models::cmus_status::CmusStatus;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn write_raw_cmus_event_to_db(db: &Connection, event: RawCmusEvent) 
-> Result<(), String> {
  match write_event(db, event) {
    Ok(State::Done) => Ok(()),
    Err(ω)          => Err(ω.to_string()),
    _               => Err("error writing raw cmus event".to_string()),
  }
}

fn write_event(db: &Connection, α: RawCmusEvent) -> Result<State, Error> {
  let query = "
    INSERT INTO raw_cmus_events (
                  time_milliseconds,
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
                )
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
  println!("{:?}", α);
  let mut statement = db.prepare(query)?;
  sql_nullable_int(   &mut statement, 1,  α.time_milliseconds)?;
  sql_int(            &mut statement, 2,  CmusStatus::to_sql_enum(α.status))?;
  sql_nullable_string(&mut statement, 3,  α.artist)?;
  sql_nullable_string(&mut statement, 4,  α.title)?;
  sql_nullable_string(&mut statement, 5,  α.album)?;
  sql_nullable_string(&mut statement, 6,  α.album_artist)?;
  sql_nullable_string(&mut statement, 7,  α.genre)?;
  sql_nullable_string(&mut statement, 8,  α.disc_number)?;
  sql_nullable_string(&mut statement, 9,  α.track_number)?;
  sql_int(            &mut statement, 10, α.duration)?;
  sql_nullable_string(&mut statement, 11, α.date)?;
  statement.next()
}
