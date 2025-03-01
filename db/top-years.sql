.headers on
.mode html
WITH ranked AS (
  SELECT albums.year AS 'Year', COUNT(plays.date) AS 'Plays' 
  FROM plays 
  JOIN albums ON (plays.album = albums.id) 
  WHERE Year <> 0 
  GROUP BY albums.year
  ORDER BY Plays DESC 
  LIMIT 10
)
SELECT
  ROW_NUMBER() OVER (ORDER BY Plays DESC) AS '#',
  Year,
  Plays
FROM ranked;
