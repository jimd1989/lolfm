.headers on
.mode html
WITH ranked AS (
  SELECT 
  artists.name AS 'Artist', 
  songs.title AS 'Song',
  (SUM(plays.duration) / 60) / 60 AS 'Hours' 
  FROM plays
  JOIN songs ON (plays.song = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
  GROUP BY songs.id 
  ORDER BY Hours DESC 
  LIMIT 15
)
SELECT
ROW_NUMBER() OVER (ORDER BY ranked.Hours DESC) AS '#', 
ranked.Artist, 
ranked.Song,
ranked.Hours
FROM ranked;
