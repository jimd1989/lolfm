.headers on
.mode html
WITH ranked AS (
  SELECT artists.name AS 'Artist', 
         COUNT(artists.id) AS 'Plays' 
    FROM plays
    JOIN songs   ON (plays.song   = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   WHERE date(plays.date, 'unixepoch', 'localtime') > date('now', '-30 days', 'localtime')
   GROUP BY artists.id 
   ORDER BY Plays DESC 
   LIMIT 50
)
SELECT ROW_NUMBER() OVER (ORDER BY ranked.Plays DESC) AS '#', 
       ranked.Artist, 
       printf('%,d', ranked.Plays) AS 'Plays'
  FROM ranked;
