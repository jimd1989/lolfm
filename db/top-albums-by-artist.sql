.headers on
.mode html
WITH ranked AS (
  SELECT artists.name AS artist,
         COUNT(albums.title) AS count
    FROM artists
    JOIN albums ON (artists.id = albums.artist)
   GROUP BY artists.id
   ORDER BY count DESC
   LIMIT 15
)
SELECT ROW_NUMBER() OVER (ORDER BY ranked.count DESC) AS '#',
       ranked.artist AS 'Artist',
       printf('%,d', ranked.count) AS 'Albums'
FROM ranked;
