use sqlite::{Connection, State, Statement};
use std::iter;

use crate::models::er::Er;
use crate::models::song::Song;

/* Partially reuses the Song model. Not sure if this is a mistake. */

pub fn get<'a>(db: &'a Connection)
-> Result<impl Iterator<Item = Result<Song, Er>> + 'a, Er> {
  let query = "
    SELECT songs.id,
           songs.title,
           artists.name
      FROM songs 
      JOIN loved   ON songs.id      = loved.song
      JOIN artists ON songs.artist  = artists.id 
     ORDER BY loved.date ASC;
  ";
  let mut statement = db.prepare(query)?;
  Ok(iter::from_fn(move || {
    match statement.next() {
      Ok(State::Row)  => Some(to_song(&mut statement)),
      Ok(State::Done) => None,
      Err(ω)          => Some(Err(Er::from(ω)))
    }
  }))
}

fn to_song(statement: &mut Statement) -> Result<Song, Er> {
  let mut s = Song::default();
  s.id     = statement.read(0)?;
  s.title  = statement.read(1)?;
  s.artist = statement.read(2)?;
  Ok(s)
}
