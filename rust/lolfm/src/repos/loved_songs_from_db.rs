use sqlite::{Connection, State, Statement};
use std::iter;

use crate::models::er::Er;
use crate::models::loved_song::LovedSong;

pub fn get<'a>(db: &'a Connection)
-> Result<impl Iterator<Item = Result<LovedSong, Er>> + 'a, Er> {
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

fn to_song(statement: &mut Statement) -> Result<LovedSong, Er> {
  let mut s = LovedSong::default();
  s.song_id = statement.read(0)?;
  s.title   = statement.read(1)?;
  s.artist  = statement.read(2)?;
  Ok(s)
}
