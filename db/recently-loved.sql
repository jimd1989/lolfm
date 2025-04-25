.headers on
.mode html
SELECT date(loved.date, 'unixepoch', 'localtime') AS 'Loved', 
       artists.name AS 'Artist', 
       songs.title AS 'Song' 
  FROM loved 
  JOIN songs   ON (loved.song   = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
 ORDER BY Loved DESC 
 LIMIT 15;
