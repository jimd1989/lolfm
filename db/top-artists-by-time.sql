.headers on
.mode html
WITH ranked AS (
  SELECT artists.name AS 'Artist', 
         (SUM(plays.duration) / 60) / 60 AS 'Hours' 
    FROM plays
    JOIN songs   ON (plays.song   = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   GROUP BY artists.id 
   ORDER BY Hours DESC 
   LIMIT 50
)
SELECT ROW_NUMBER() OVER (ORDER BY ranked.Hours DESC) AS '#', 
       ranked.Artist, 
       printf('%,d', ranked.Hours) AS 'Hours'
  FROM ranked;
