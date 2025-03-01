.headers on
.mode html
WITH ranked AS (
  SELECT 
    artists.name AS 'Artist',
    albums.title AS 'Title',
    COUNT(plays.date) AS 'Plays'
  FROM plays
  JOIN albums ON (plays.album = albums.id)
  JOIN artists ON (albums.artist = artists.id)
  GROUP BY albums.id
  ORDER BY Plays DESC 
  LIMIT 15
)
SELECT
ROW_NUMBER() OVER (ORDER BY ranked.Plays DESC) AS '#', 
ranked.Artist, 
ranked.Title,
ranked.Plays
FROM ranked;
