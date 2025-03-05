.headers on
.mode html
WITH q AS (
  SELECT ROW_NUMBER() OVER (ORDER BY loved.date) AS 'd',
         date(loved.date, 'unixepoch', 'localtime') AS 'Loved', 
         artists.name AS 'Artist', 
         songs.title AS 'Song' 
    FROM loved 
    JOIN songs   ON (loved.song   = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   ORDER BY d DESC 
   LIMIT 15
)
SELECT printf('%,d', d) AS '#', Loved, Artist, Song FROM q;
