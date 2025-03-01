.headers on
.mode html
WITH ranked AS (
  SELECT albums.year AS 'Year', (SUM(plays.duration) / 60) / 60 AS 'Hours' 
  FROM plays 
  JOIN albums ON (plays.album = albums.id) 
  WHERE Year <> 0 
  GROUP BY albums.year
  ORDER BY Hours DESC 
  LIMIT 10
)
SELECT
  ROW_NUMBER() OVER (ORDER BY Hours DESC) AS '#',
  Year,
  Hours
FROM ranked;
