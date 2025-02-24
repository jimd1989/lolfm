use sqlite::Connection;

use crate::helpers::sql_helpers::{
  sql_execute_void, sql_int, sql_nullable_string
};
use crate::models::cmus_status::CmusStatus;
use crate::models::er::Er;
use crate::models::cmus_event::CmusEvent;

pub fn write(db: &Connection, ω: impl Iterator<Item = Result<CmusEvent, Er>>)
-> Result<(), Er> {
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
  let mut statement = db.prepare(query)?;
  for e in ω {
    let α = e?;
    sql_int(            &mut statement, 1,  α.time_milliseconds)?;
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
    sql_execute_void(   &mut statement)?;
    statement.reset()?;
  }
  Ok(())
}
