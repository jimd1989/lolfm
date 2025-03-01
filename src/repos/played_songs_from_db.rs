use sqlite::{Connection, State, Statement};
use std::iter;

use crate::models::er::Er;
use crate::models::played_song::PlayedSong;
use crate::models::timestamp::Seconds;

pub fn get<'a>(db: &'a Connection)
-> Result<impl Iterator<Item = Result<PlayedSong, Er>> + 'a, Er> {
  let query = "
    SELECT plays.date, 
           plays.duration, 
           songs.title,
           artists.name, 
           albums.title,
           albums.year,
           genres.name,
           album_artists.name
      FROM plays 
      JOIN songs                    ON plays.song    = songs.id
      JOIN artists                  ON songs.artist  = artists.id 
      JOIN albums                   ON plays.album   = albums.id 
      JOIN genres                   ON songs.genre   = genres.id 
      JOIN artists AS album_artists ON albums.artist = album_artists.id 
     ORDER BY date ASC;
  ";
  let mut statement = db.prepare(query)?;
  Ok(iter::from_fn(move || {
    match statement.next() {
      Ok(State::Row)  => Some(to_played_song(&mut statement)),
      Ok(State::Done) => None,
      Err(ω)          => Some(Err(Er::from(ω)))
    }
  }))
}

fn to_played_song(statement: &mut Statement) -> Result<PlayedSong, Er> {
  let raw_time: i64 = statement.read(0)?;
  let played_song = PlayedSong {
    date:         Seconds::from_i64(raw_time),
    duration:     statement.read(1)?,
    title:        statement.read(2)?,
    artist:       statement.read(3)?,
    album:        statement.read(4)?,
    year:         statement.read(5)?,
    genre:        statement.read(6)?,
    album_artist: statement.read(7)?,
  };
  Ok(played_song)
}
