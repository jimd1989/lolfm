.headers on
.mode html
WITH ranked AS (
  SELECT artists.name AS 'Artist',
         albums.title AS 'Title',
         COUNT(plays.date) AS 'Plays'
    FROM plays
    JOIN albums  ON (plays.album   = albums.id)
    JOIN artists ON (albums.artist = artists.id)
   WHERE date(plays.date, 'unixepoch', 'localtime') > date('now', '-11 months', 'localtime')
   GROUP BY albums.id
   ORDER BY Plays DESC 
   LIMIT 15
)
SELECT ROW_NUMBER() OVER (ORDER BY ranked.Plays DESC) AS '#', 
       ranked.Artist, 
       ranked.Title,
       printf('%,d', ranked.Plays) AS 'Plays'
  FROM ranked;
