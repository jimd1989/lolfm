.mode list
WITH top_plays AS (
 SELECT countries.name, COUNT(plays.song) AS 'Plays'
   FROM plays
   JOIN songs     ON plays.song      = songs.id
   JOIN artists   ON songs.artist    = artists.id
   JOIN countries ON artists.country = countries.id
  GROUP BY countries.name
)
SELECT COUNT(top_plays.name)
 FROM top_plays
WHERE top_plays.Plays <> 0;
