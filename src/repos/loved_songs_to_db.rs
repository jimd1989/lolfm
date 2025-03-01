use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int, sql_string};
use crate::models::er::Er;
use crate::models::loved_song::LovedSong;

pub fn write(db: &Connection, ω: impl Iterator<Item = Result<LovedSong, Er>>) 
-> Result<(), Er> {
  let query = "
    INSERT INTO loved(date, song)
    SELECT ?, songs.id
      FROM songs
      JOIN artists ON songs.artist = artists.id
     WHERE (songs.title = ?)
       AND (artists.name = ?)
        ON CONFLICT DO NOTHING
  ";
  let mut statement = db.prepare(query)?;
  for res in ω {
    let α = res?;
    sql_int(   &mut statement, 1, α.date.to_i64())?;
    sql_string(&mut statement, 2, α.title)?; 
    sql_string(&mut statement, 3, α.artist)?; 
    sql_execute_void(&mut statement)?;
    statement.reset()?;
  }
  Ok(())
}
