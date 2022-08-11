.headers on
.mode html
WITH ranked AS (
  SELECT 
  artists.name AS 'Artist', 
  songs.title AS 'Song',
  COUNT(songs.id) AS 'Plays' FROM plays
  JOIN songs ON (plays.song = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
  GROUP BY songs.id 
  ORDER BY Plays DESC 
  LIMIT 20
)
SELECT
ROW_NUMBER() OVER (ORDER BY ranked.Plays DESC) AS '#', 
ranked.Artist, 
ranked.Song,
ranked.Plays
FROM ranked;

