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
    ";
  let genres_query = "
    INSERT INTO genres(name)
    VALUES (?)
        ON CONFLICT(name)
        DO UPDATE SET name=excluded.name
    ";
  let albums_query = "
      WITH album_artist AS (SELECT id FROM artists WHERE name=?)
    INSERT INTO albums(title, artist, year)
    VALUES (?, (SELECT id FROM album_artist), ?)
        ON CONFLICT(title, artist)
        DO UPDATE SET year=excluded.year
  ";
  let songs_query = "
      WITH artist AS (SELECT id FROM artists WHERE name=?),
           genre  AS (SELECT id FROM  genres WHERE name=?)
    INSERT INTO songs(title, artist, genre)
    VALUES (?, (SELECT id FROM artist), (SELECT id FROM genre))
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
        sql_string(&mut artists_statement, 1, α.artist.clone())?;
        sql_execute_void(&mut artists_statement)?;
        artists_statement.reset()?;

        sql_string(&mut artists_statement, 1, α.album_artist.clone())?;
        sql_execute_void(&mut artists_statement)?;
        artists_statement.reset()?;

        sql_string(&mut genres_statement, 1, α.genre.clone())?;
        sql_execute_void(&mut genres_statement)?;
        genres_statement.reset()?;

        sql_string(&mut albums_statement, 1, α.album_artist)?;
        sql_string(&mut albums_statement, 2, α.album)?;
        sql_int   (&mut albums_statement, 3, α.year)?;
        sql_execute_void(&mut albums_statement)?;
        albums_statement.reset()?;

        sql_string(&mut songs_statement, 1, α.artist)?;
        sql_string(&mut songs_statement, 2, α.genre)?;
        sql_string(&mut songs_statement, 3, α.title)?;
        sql_execute_void(&mut songs_statement)?;
        songs_statement.reset()?;
      }
      Err(β) => { return Err(β); }
    }
  }
  Ok(())
}
