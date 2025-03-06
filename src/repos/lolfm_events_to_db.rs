use sqlite::Connection;

use crate::helpers::sql_helpers::{sql_execute_void, sql_int, sql_string};
use crate::models::er::Er;
use crate::models::lolfm_event::LolfmEvent;

pub fn write(db: &Connection, ω: impl Iterator<Item = Result<LolfmEvent, Er>>) 
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
  RETURNING albums.id
  ";
  let songs_query = "
    INSERT INTO songs(artist, genre, title)
    VALUES (?, ?, ?)
        ON CONFLICT(title, artist)
        DO UPDATE SET genre=excluded.genre
  RETURNING songs.id
  ";
  let plays_query = "
  INSERT INTO plays(song, album, date, duration)
  VALUES (?, ?, ?, ?)
  ";
  let delete_query = "
  DELETE FROM raw_cmus_events
   WHERE time_milliseconds < ?
  ";

  let mut artists_statement = db.prepare(artists_query)?;
  let mut genres_statement  = db.prepare(genres_query)?;
  let mut albums_statement  = db.prepare(albums_query)?;
  let mut songs_statement   = db.prepare(songs_query)?;
  let mut plays_statement   = db.prepare(plays_query)?;
  let mut delete_statement  = db.prepare(delete_query)?;

  for res in ω {
    match res {
      Ok(LolfmEvent::RecordPlay(time, α)) => {
        sql_string(&mut artists_statement, 1, α.artist.clone())?;
        artists_statement.next()?;
        let artist_id: i64 = artists_statement.read(0)?;
        artists_statement.reset()?;

        sql_string(&mut artists_statement, 1, α.album_artist.clone())?;
        artists_statement.next()?;
        let album_artist_id: i64 = artists_statement.read(0)?;
        artists_statement.reset()?;

        sql_string(&mut genres_statement, 1, α.genre.clone())?;
        genres_statement.next()?;
        let genre_id: i64 = genres_statement.read(0)?;
        genres_statement.reset()?;

        sql_int   (&mut albums_statement, 1, album_artist_id)?;
        sql_string(&mut albums_statement, 2, α.album.clone())?;
        sql_int   (&mut albums_statement, 3, α.year)?;
        albums_statement.next()?;
        let album_id: i64 = albums_statement.read(0)?;
        albums_statement.reset()?;

        sql_int   (&mut songs_statement, 1, artist_id)?;
        sql_int   (&mut songs_statement, 2, genre_id)?;
        sql_string(&mut songs_statement, 3, α.title.clone())?;
        songs_statement.next()?;
        let song_id: i64 = songs_statement.read(0)?;
        songs_statement.reset()?;

        sql_int(&mut plays_statement, 1, song_id)?;
        sql_int(&mut plays_statement, 2, album_id)?;
        sql_int(&mut plays_statement, 3, time.to_i64())?;
        sql_int(&mut plays_statement, 4, α.duration)?;
        sql_execute_void(&mut plays_statement)?;
        plays_statement.reset()?;
      }
      Ok(LolfmEvent::DeleteBefore(time)) => {
        sql_int   (&mut delete_statement, 1, time.to_i64())?;
        sql_execute_void(&mut delete_statement)?;
        delete_statement.reset()?;
      }
      Err(β) => { return Err(β); }
    }
  }
  Ok(())
}
