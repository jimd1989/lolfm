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

WITH 
full_matches AS (
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

WITH windowed AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY lastfm_dump.artist
    ORDER BY lastfm_dump.date
  ) AS ranking, lastfm_dump.date, lastfm_dump.artist, lastfm_dump.title, lastfm_dump.album
  FROM lastfm_dump
)
SELECT * FROM windowed;
--WHERE artist = '椎名林檎';

WITH
lagged AS (
  SELECT LAG(lastfm_dump.artist) OVER (
    ORDER BY lastfm_dump.date
  ) AS lagged_artist, 
  lastfm_dump.date, lastfm_dump.artist, lastfm_dump.title, lastfm_dump.album
  FROM lastfm_dump
),
windowed AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY (lagged.lagged_artist = lagged.artist), artist
    ORDER BY lagged.artist
  ) AS n, lagged.date, lagged.artist, lagged.title, lagged.album
  FROM lagged
)
SELECT * FROM lagged;

WITH 
ids AS (
  SELECT ROW_NUMBER() OVER (ORDER BY lastfm_dump.date) AS id,
  lastfm_dump.date, lastfm_dump.artist, lastfm_dump.title, lastfm_dump.album
  FROM lastfm_dump
),
windowed AS (
  SELECT ROW_NUMBER() OVER(PARTITION BY artist, album, (id - rn) ORDER BY id) AS rn,
  date, artist, title, album
  FROM (SELECT ids.*, ROW_NUMBER() OVER (PARTITION BY artist ORDER BY id) AS rn FROM ids)
)
SELECT * FROM windowed WHERE album=''
ORDER BY date;

-- Won't work for various artists
WITH
library_ids AS (
  SELECT ROW_NUMBER() OVER (ORDER BY date) AS id, * FROM library_dump
),
library_tracks AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY artist, album, date, id - row ORDER BY id
  ) AS track, *
  FROM (
    SELECT library_ids.*, ROW_NUMBER() OVER (
      PARTITION BY artist, album, date ORDER BY id
    ) AS row FROM library_ids
  WHERE album_artist <> 'Various Artists'
  )
),
lastfm_ids AS (
  SELECT ROW_NUMBER() OVER (ORDER BY date) AS id, * FROM lastfm_dump
),
lastfm_tracks AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY artist, album, id - row ORDER BY id
  ) AS track, *
  FROM (
    SELECT lastfm_ids.*, ROW_NUMBER() OVER (
      PARTITION BY artist, album ORDER BY id
    ) AS row FROM lastfm_ids
  WHERE album = ''
  )
),
ranked_albums AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY artist, title ORDER BY date
  ) AS ranking, *
  FROM library_dump
),
perfect_matches AS (
  SELECT 
    library_dump.duration, library_dump.title, library_dump.album,
    library_dump.artist, library_dump.album_artist, library_dump.genre,
    lastfm_dump.date  
  FROM lastfm_dump
  JOIN library_dump ON (
    (library_dump.artist = lastfm_dump.artist COLLATE NOCASE) AND 
    (library_dump.title = lastfm_dump.title COLLATE NOCASE) AND 
    (library_dump.album = lastfm_dump.album COLLATE NOCASE)
  ) 
),
track_matches AS (
  SELECT
    library_tracks.duration, library_tracks.title, library_tracks.album,
    library_tracks.artist, library_tracks.album_artist, library_tracks.genre,
    lastfm_tracks.date
  FROM lastfm_tracks
  JOIN library_tracks ON (
    (library_tracks.track = lastfm_tracks.track) AND
    (library_tracks.artist = lastfm_tracks.artist COLLATE NOCASE) AND 
    (library_tracks.title = lastfm_tracks.title COLLATE NOCASE)
  )
),
best_matches AS (
  SELECT
    ranked_albums.duration, ranked_albums.title, ranked_albums.album,
    ranked_albums.artist, ranked_albums.album_artist, ranked_albums.genre,
    lastfm_dump.date  
  FROM lastfm_dump
  JOIN ranked_albums
  ON (
    (ranked_albums.artist = lastfm_dump.artist COLLATE NOCASE) AND 
    (ranked_albums.title = lastfm_dump.title COLLATE NOCASE)
  )
  WHERE ranked_albums.ranking = 1
),
misspelled_titles AS (
  SELECT
    library_tracks.duration, library_tracks.title, library_tracks.album,
    library_tracks.artist, library_tracks.album_artist, library_tracks.genre,
    lastfm_tracks.date
  FROM lastfm_tracks
  JOIN library_tracks ON (
    (library_tracks.track = lastfm_tracks.track) AND
    (library_tracks.artist = lastfm_tracks.artist COLLATE NOCASE)
  )
),
-- orphans
-- need to enforce uniqueness over union
all_matches AS (
  SELECT * FROM perfect_matches UNION 
  SELECT * FROM track_matches UNION
  SELECT * FROM best_matches UNION
  SELECT * FROM misspelled_titles
)
SELECT COUNT(duration) FROM all_matches;
