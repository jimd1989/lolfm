.headers on
.mode table
WITH top_time AS (
  SELECT artists.name, (SUM(plays.duration) / 60) / 60 AS 'Hours' 
  FROM plays
  JOIN songs ON (plays.song = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
  GROUP BY artists.id 
  ORDER BY Hours DESC 
  LIMIT 50
),
time_ranked AS (
  SELECT
  ROW_NUMBER() OVER (ORDER BY top_time.Hours DESC) AS 'row', 
  name, 
  Hours
  FROM top_time
),
top_plays AS (
  SELECT artists.name, COUNT(artists.id) AS 'Plays' FROM plays
  JOIN songs ON (plays.song = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
  GROUP BY artists.id 
  ORDER BY Plays DESC 
  LIMIT 50
),
plays_ranked AS (
  SELECT
  ROW_NUMBER() OVER (ORDER BY top_plays.Plays DESC) AS 'row', 
  name, 
  Plays
  FROM top_plays
)
SELECT
time_ranked.row AS '#',
plays_ranked.name AS 'Artist (Plays)',
plays_ranked.Plays,
time_ranked.name AS 'Artist (Time)',
time_ranked.Hours
FROM time_ranked JOIN plays_ranked ON (time_ranked.row = plays_ranked.row);
