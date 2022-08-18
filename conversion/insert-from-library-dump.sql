-- to master_dump
WITH
library_ids AS (
  SELECT ROW_NUMBER() OVER (ORDER BY year) AS id, * FROM library_dump
),
library_tracks AS (
  SELECT ROW_NUMBER() OVER (
    PARTITION BY artist, album, year, id - row ORDER BY id
  ) AS track, *
  FROM (
    SELECT library_ids.*, ROW_NUMBER() OVER (
      PARTITION BY artist, album, year ORDER BY id
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
    PARTITION BY artist, title ORDER BY year
  ) AS ranking, *
  FROM library_dump
),
perfect_matches AS (
  SELECT 
    library_dump.duration, library_dump.title, library_dump.album,
    library_dump.artist, library_dump.album_artist, library_dump.genre,
    library_dump.year, lastfm_dump.date, 1 AS optimality
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
    library_tracks.year, lastfm_tracks.date, 2 AS optimality
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
    ranked_albums.year, lastfm_dump.date, 3 AS optimality
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
    library_tracks.year, lastfm_tracks.date, 4 AS optimality
  FROM lastfm_tracks
  JOIN library_tracks ON (
    (library_tracks.track = lastfm_tracks.track) AND
    (library_tracks.artist = lastfm_tracks.artist COLLATE NOCASE)
  )
),
fallback AS (
  SELECT
    0 AS duration, lastfm_dump.title, 'Unknown Album' AS album,
    lastfm_dump.artist, lastfm_dump.artist AS album_artist,
    'Unknown Genre' AS genre, 0 AS year, lastfm_dump.date, 5 AS optimality
  FROM lastfm_dump
),
all_matches AS (
  SELECT * FROM perfect_matches UNION 
  SELECT * FROM track_matches UNION
  SELECT * FROM best_matches UNION
  SELECT * FROM misspelled_titles UNION
  SELECT * FROM fallback
),
unique_matches AS (
  SELECT duration, title, album, artist, album_artist, genre, year, date
  FROM (
    SELECT ROW_NUMBER() OVER (PARTITION BY date ORDER BY optimality) AS rank, *
    FROM all_matches
  )
  WHERE rank = 1
),
orphans AS (
  SELECT * FROM lastfm_dump 
  WHERE lastfm_dump.date NOT IN (SELECT date FROM unique_matches)
)
INSERT INTO master_dump SELECT * FROM unique_matches;

-- from master_dump to plays
BEGIN TRANSACTION;
  INSERT INTO artists(name) SELECT artist FROM master_dump WHERE true
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO artists(name) SELECT album_artist FROM master_dump WHERE true
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO genres(name) SELECT genre FROM master_dump WHERE true
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO albums(title, artist, year)
  SELECT DISTINCT master_dump.album, artists.id, master_dump.year
  FROM master_dump
  JOIN artists
  ON (master_dump.album_artist=artists.name COLLATE NOCASE) WHERE true
  ON CONFLICT(title, artist) DO UPDATE SET year=excluded.year;

  INSERT INTO songs(title, artist, genre)
  SELECT 
    master_dump.title, artists.id, genres.id
  FROM master_dump
  JOIN artists ON (master_dump.artist = artists.name COLLATE NOCASE)
  JOIN genres ON (master_dump.genre = genres.name COLLATE NOCASE)
  WHERE true
  ON CONFLICT(title, artist)
  DO UPDATE SET genre=excluded.genre;

  WITH
  matched_songs AS (
    SELECT master_dump.date, songs.id, master_dump.duration FROM master_dump
    JOIN artists ON (master_dump.artist = artists.name COLLATE NOCASE)
    JOIN songs ON ((artists.id = songs.artist) AND 
                   (master_dump.title = songs.title COLLATE NOCASE))
  ),
  matched_albums AS (
    SELECT master_dump.date, albums.id FROM master_dump
    JOIN artists ON (master_dump.album_artist = artists.name COLLATE NOCASE)
    JOIN albums ON ((artists.id = albums.artist) AND
                    (master_dump.album = albums.title COLLATE NOCASE))
  )
  INSERT INTO plays(date, song, album, duration)
  SELECT matched_songs.date, matched_songs.id,
         matched_albums.id, matched_songs.duration
  FROM matched_songs
  JOIN matched_albums ON (matched_songs.date = matched_albums.date);
COMMIT;
