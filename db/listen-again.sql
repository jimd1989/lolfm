.headers on
.mode html
WITH album_plays AS (
  SELECT COUNT(plays.date) as 'count', 
         date(plays.date, 'unixepoch', 'localtime') AS 'last',
         artists.name,
         albums.title
    FROM albums 
    LEFT JOIN plays   ON (albums.id     = plays.album)
    JOIN      artists ON (albums.artist = artists.id)
   WHERE albums.title <> 'Unknown Album'
   GROUP BY albums.id
)
SELECT album_plays.name AS 'Artist',
       album_plays.title AS 'Album',
       last AS 'Last Played'
  FROM album_plays
 WHERE count < 2
 ORDER BY RANDOM()
 LIMIT 15;
