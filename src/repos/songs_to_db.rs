use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int, sql_string};
use crate::models::er::Er;
use crate::models::song::Song;

pub fn write(db: &Connection, ω: impl Iterator<Item = Result<Song, Er>>) 
-> Result<(), Er> {
  let artists_query = "
       INSERT INTO artists(name)
       VALUES (?)
           ON CONFLICT(name)
           DO UPDATE SET name=excluded.name
    RETURNING artists.id
    ";
  let genres_query = "
       INSERT INTO genres(name)
       VALUES (?)
           ON CONFLICT(name)
           DO UPDATE SET name=excluded.name
    RETURNING genres.id
    ";
  let albums_query = "
  INSERT INTO albums(artist, title, year)
  VALUES (?, ?, ?)
      ON CONFLICT(title, artist)
      DO UPDATE SET year=excluded.year
  ";
  let songs_query = "
  INSERT INTO songs(artist, genre, title)
  VALUES (?, ?, ?)
     ON CONFLICT(title, artist)
     DO UPDATE SET genre=excluded.genre
  ";
  let mut artists_statement = db.prepare(artists_query)?;
  let mut genres_statement  = db.prepare(genres_query)?;
  let mut albums_statement  = db.prepare(albums_query)?;
  let mut songs_statement   = db.prepare(songs_query)?;

  for res in ω {
    match res {
      Ok(α) => {
        sql_string(&mut artists_statement, 1, α.artist)?;
        artists_statement.next()?;
        let artist_id: i64 = artists_statement.read(0)?;
        artists_statement.reset()?;

        sql_string(&mut artists_statement, 1, α.album_artist)?;
        artists_statement.next()?;
        let album_artist_id: i64 = artists_statement.read(0)?;
        artists_statement.reset()?;

        sql_string(&mut genres_statement, 1, α.genre)?;
        genres_statement.next()?;
        let genre_id: i64 = genres_statement.read(0)?;
        genres_statement.reset()?;

        sql_int   (&mut albums_statement, 1, album_artist_id)?;
        sql_string(&mut albums_statement, 2, α.album)?;
        sql_int   (&mut albums_statement, 3, α.year)?;
        sql_execute_void(&mut albums_statement)?;
        albums_statement.reset()?;

        sql_int   (&mut songs_statement, 1, artist_id)?;
        sql_int   (&mut songs_statement, 2, genre_id)?;
        sql_string(&mut songs_statement, 3, α.title)?;
        sql_execute_void(&mut songs_statement)?;
        songs_statement.reset()?;
      }
      Err(β) => { return Err(β); }
    }
  }
  Ok(())
}
