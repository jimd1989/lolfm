.headers on
.mode html
WITH top_time AS (
 SELECT countries.name, (SUM(plays.duration) / 60) / 60 AS 'Hours'
   FROM plays
   JOIN songs     ON plays.song      = songs.id
   JOIN artists   ON songs.artist    = artists.id
   JOIN countries ON artists.country = countries.id
  GROUP BY countries.name
  ORDER BY hours DESC
  LIMIT 15
),
time_ranked AS (
  SELECT ROW_NUMBER() OVER (ORDER BY top_time.Hours DESC) AS 'row', name, Hours
    FROM top_time
)
  SELECT time_ranked.row AS '#',
         time_ranked.name AS 'Country',
         printf('%,d', time_ranked.Hours) AS 'Hours'
    FROM time_ranked;
