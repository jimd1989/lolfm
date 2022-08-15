.headers on
.mode html
WITH top_time AS (
  SELECT genres.name, (SUM(plays.duration) / 60) / 60 AS 'Hours' FROM plays
  JOIN songs ON (plays.song = songs.id) 
  JOIN genres ON (songs.genre = genres.id) 
  WHERE genres.name <> 'Unknown Genre'
  GROUP BY genres.id 
  ORDER BY Hours DESC 
  LIMIT 15
),
time_ranked AS (
  SELECT
  ROW_NUMBER() OVER (ORDER BY top_time.Hours DESC) AS 'row', 
  name,
  Hours
  FROM top_time
)
SELECT
time_ranked.row AS '#',
time_ranked.name AS 'Genre',
time_ranked.Hours
FROM time_ranked;
