.headers on
.mode html
SELECT date(plays.date, 'unixepoch', 'localtime') AS 'Date',
       artists.name AS 'Artist',
       albums.title AS 'Title'
  FROM albums 
  JOIN plays   ON (albums.id     = plays.album)
  JOIN artists ON (albums.artist = artists.id)
 GROUP BY albums.id
 ORDER BY date DESC
 LIMIT 15;
