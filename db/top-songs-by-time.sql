.headers on
.mode html
WITH ranked AS (
  SELECT artists.name AS 'Artist', 
         IIF(EXISTS (SELECT 1 FROM loved WHERE loved.song = songs.id), '♥', '') AS '♥',
         songs.title AS 'Song',
         (SUM(plays.duration) / 60) / 60 AS 'Hours' 
    FROM plays
    JOIN songs   ON (plays.song   = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   GROUP BY songs.id 
   ORDER BY Hours DESC 
   LIMIT 15
)
SELECT ROW_NUMBER() OVER (ORDER BY ranked.Hours DESC) AS '#', 
       ranked.Artist, 
       ranked.♥,
       ranked.Song,
       printf('%,d', ranked.Hours) AS 'Hours'
  FROM ranked;
