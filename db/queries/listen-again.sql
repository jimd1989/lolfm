.headers on
.mode html
WITH album_plays AS (
  SELECT COUNT(plays.date) AS 'count', plays.album
  FROM plays 
  GROUP BY plays.album
),
underplayed AS (
  SELECT artists.name, albums.title, albums.id
  FROM album_plays 
  JOIN albums ON (album_plays.album = albums.id) 
  JOIN artists ON (albums.artist = artists.id) 
  WHERE count < 2 AND albums.title <> 'Unknown Album' 
  ORDER BY RANDOM() 
  LIMIT 15
)
SELECT 
  underplayed.name AS 'Artist',
  underplayed.title AS 'Album',
  date(plays.date, 'unixepoch', 'localtime') AS 'Last Played'
FROM underplayed
JOIN plays ON (underplayed.id = plays.album)
GROUP BY underplayed.id;
