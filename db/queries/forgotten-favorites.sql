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
  GROUP BY last_played.id
),
scored AS (
  SELECT
    name,
    ago * count AS 'score',
    datetime(date, 'unixepoch', 'localtime') AS 'last'
  FROM last_played_counts ORDER BY score DESC LIMIT 20
)
SELECT
  ROW_NUMBER() OVER (ORDER BY score DESC) AS '#', 
  name AS 'Artist', 
  last AS 'Last Played' 
FROM scored;
