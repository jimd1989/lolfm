.headers on
.mode html
WITH top_plays AS (
  SELECT genres.name, COUNT(genres.id) AS 'Plays' FROM plays
  JOIN songs ON (plays.song = songs.id) 
  JOIN genres ON (songs.genre = genres.id) 
  WHERE genres.name <> 'Unknown Genre'
  GROUP BY genres.id 
  ORDER BY Plays DESC 
  LIMIT 25
),
plays_ranked AS (
  SELECT
  ROW_NUMBER() OVER (ORDER BY top_plays.Plays DESC) AS 'row', 
  name, 
  Plays
  FROM top_plays
)
SELECT
plays_ranked.row AS '#',
plays_ranked.name AS 'Genre',
plays_ranked.Plays
FROM plays_ranked;
