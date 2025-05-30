.headers on
.mode html
WITH ranked AS (
  SELECT artists.name AS 'Artist', 
         IIF(EXISTS (SELECT 1 FROM loved WHERE loved.song = songs.id), '♥', '') AS '♥',
         songs.title AS 'Song',
         COUNT(songs.id) AS 'Plays' FROM plays
    JOIN songs   ON (plays.song   = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   WHERE date(plays.date, 'unixepoch', 'localtime') > date('now', '-11 months', 'localtime')
   GROUP BY songs.id 
   ORDER BY Plays DESC 
   LIMIT 15
)
SELECT ROW_NUMBER() OVER (ORDER BY ranked.Plays DESC) AS '#', 
       ranked.Artist,
       ranked.♥,
       ranked.Song,
       printf('%,d', ranked.Plays) AS 'Plays'
  FROM ranked;
