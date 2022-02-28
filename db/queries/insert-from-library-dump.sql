BEGIN TRANSACTION;
  INSERT INTO artists(name) SELECT artist FROM library_dump WHERE true
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO artists(name) SELECT album_artist FROM library_dump WHERE true
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO genres(name) SELECT genre FROM library_dump WHERE true
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO albums(title, artist, year)
  SELECT DISTINCT library_dump.album, artists.id, library_dump.date 
  FROM library_dump 
  JOIN artists ON library_dump.album_artist=artists.name WHERE true
  ON CONFLICT(title, artist) DO UPDATE SET year=excluded.year;

  INSERT INTO songs(title, artist, album, genre, duration)
  SELECT 
    library_dump.title, artists.id, albums.id, genres.id, library_dump.duration
  FROM library_dump 
  JOIN artists ON library_dump.artist = artists.name 
  JOIN genres ON library_dump.genre = genres.name WHERE true
  ON CONFLICT(title, artist)
  DO UPDATE SET genre=excluded.genre, duration=excluded.duration;
COMMIT;


-- Scratchpad

WITH full_matches AS (
  SELECT lastfm_dump.date, library_dump.artist, library_dump.title, 
         library_dump.album 
  FROM library_dump 
  JOIN lastfm_dump 
  ON (
    (library_dump.artist = lastfm_dump.artist COLLATE NOCASE) AND 
    (library_dump.title = lastfm_dump.title COLLATE NOCASE) AND 
    (library_dump.album = lastfm_dump.album COLLATE NOCASE)
  )
  ORDER BY lastfm_dump.date
),
ranked AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY library_dump.artist, library_dump.title
    ORDER BY library_dump.date
  ) AS ranking, 
  library_dump.artist, library_dump.title, library_dump.album 
  FROM library_dump
),
best_matches AS (
  SELECT lastfm_dump.date, ranked.artist, ranked.title, ranked.album
  FROM ranked
  JOIN lastfm_dump
  ON (
    (ranked.artist = lastfm_dump.artist COLLATE NOCASE) AND 
    (ranked.title = lastfm_dump.title COLLATE NOCASE) AND
    ranked.ranking = 1
  )
  WHERE lastfm_dump.album = ''
),
unmatched AS (
  SELECT lastfm_dump.date, lastfm_dump.artist, lastfm_dump.title,
         lastfm_dump.album
  FROM lastfm_dump
  WHERE (
    lastfm_dump.artist COLLATE NOCASE NOT IN (SELECT artist FROM library_dump)
  )
)
SELECT DISTINCT artist, title, album FROM unmatched;
