.headers on
.mode html
WITH ranked AS (
  SELECT 
    artists.name AS 'Artist',
    albums.title AS 'Title',
    (SUM(plays.duration) / 60) / 60 AS 'Hours'
  FROM plays
  JOIN albums ON (plays.album = albums.id)
  JOIN artists ON (albums.artist = artists.id)
  GROUP BY albums.id
  ORDER BY Hours DESC 
  LIMIT 15
)
SELECT
ROW_NUMBER() OVER (ORDER BY ranked.Hours DESC) AS '#', 
ranked.Artist, 
ranked.Title,
ranked.Hours
FROM ranked;
