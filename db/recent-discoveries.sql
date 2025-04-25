.headers on
.mode html
  WITH last_albums AS (
SELECT printf('%,d', ROW_NUMBER() OVER (ORDER BY plays.date)) AS '#',
       date(plays.date, 'unixepoch', 'localtime') AS 'Date',
       artists.name AS 'Artist',
       albums.title AS 'Title'
  FROM albums 
  JOIN plays   ON (albums.id     = plays.album)
  JOIN artists ON (albums.artist = artists.id)
 GROUP BY albums.id
 ORDER BY date DESC
 LIMIT 15)
SELECT "#", Date, Artist, Title
  FROM last_albums
 ORDER BY "#" DESC;
