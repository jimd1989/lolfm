.headers on
.mode html
SELECT printf('%,d', ROW_NUMBER() OVER (ORDER BY date)) AS '#',
       strftime('%Y-%m-%d %H:%M', plays.date, 'unixepoch', 'localtime') AS 'Time',
       artists.name AS 'Artist', 
       IIF(EXISTS (SELECT 1 FROM loved WHERE loved.song = songs.id), '♥', '') AS '♥',
       songs.title AS 'Song'
  FROM plays 
  JOIN songs   ON (plays.song = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
 ORDER BY date DESC 
 LIMIT 15;
