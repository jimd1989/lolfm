.headers on
.mode html
WITH played AS (
  SELECT date, artists.id, artists.name
  FROM plays 
  JOIN songs ON (plays.song = songs.id) 
  JOIN artists ON (songs.artist = artists.id) 
  ORDER BY date DESC), 
last_played AS (
  SELECT date, id, name FROM played 
  GROUP BY id
), 
last_played_counts AS (
  SELECT 
    last_played.date,
    strftime('%s', 'now') - last_played.date AS 'ago',
    COUNT(played.date) AS 'count', -- Use log() for older stuff
    last_played.name,
    last_played.id
  FROM last_played 
  JOIN played ON (last_played.id = played.id) 
  WHERE date(last_played.date, 'unixepoch', 'localtime') 
    < date('now', '-36 months', 'localtime')
  GROUP BY last_played.id
),
scored AS (
  SELECT
    name,
    ago * count AS 'score',
    date(date, 'unixepoch', 'localtime') AS 'last'
  FROM last_played_counts ORDER BY score DESC LIMIT 50
)
SELECT
  name AS 'Artist', 
  last AS 'Last Played' 
FROM scored
ORDER BY RANDOM()
LIMIT 15;
