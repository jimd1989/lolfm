.headers on
.mode html
WITH top_plays AS (
 SELECT countries.name, COUNT(plays.song) AS 'Plays'
   FROM plays
   JOIN songs     ON plays.song      = songs.id
   JOIN artists   ON songs.artist    = artists.id
   JOIN countries ON artists.country = countries.id
  GROUP BY countries.name
  ORDER BY Plays DESC
  LIMIT 15
),
plays_ranked AS (
  SELECT ROW_NUMBER() OVER (ORDER BY top_plays.Plays DESC) AS 'row', name, Plays
    FROM top_plays
)
  SELECT plays_ranked.row AS '#',
         plays_ranked.name AS 'Country',
         printf('%,d', plays_ranked.Plays) AS 'Plays'
    FROM plays_ranked;
