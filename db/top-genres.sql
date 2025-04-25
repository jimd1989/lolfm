.headers on
.mode html
WITH top_plays AS (
  SELECT genres.name, COUNT(genres.id) AS 'Plays'
    FROM plays
    JOIN songs  ON (plays.song  = songs.id) 
    JOIN genres ON (songs.genre = genres.id) 
   WHERE genres.name <> 'Unknown Genre'
   GROUP BY genres.id 
   ORDER BY Plays DESC 
   LIMIT 25
),
plays_ranked AS (
  SELECT ROW_NUMBER() OVER (ORDER BY top_plays.Plays DESC) AS 'row', 
         name, 
         Plays
    FROM top_plays
),
top_time AS (
  SELECT genres.name, (SUM(plays.duration) / 60) / 60 AS 'Hours'
    FROM plays
    JOIN songs  ON (plays.song  = songs.id) 
    JOIN genres ON (songs.genre = genres.id) 
   WHERE genres.name <> 'Unknown Genre'
   GROUP BY genres.id 
   ORDER BY Hours DESC 
   LIMIT 25
),
time_ranked AS (
  SELECT ROW_NUMBER() OVER (ORDER BY top_time.Hours DESC) AS 'row', 
         name,
         Hours
    FROM top_time
)
SELECT plays_ranked.row AS '#',
       plays_ranked.name AS 'Genre (Plays)',
       plays_ranked.Plays,
       time_ranked.name AS 'Genre (Time)',
       printf('%,d', time_ranked.Hours) AS 'Hours'
  FROM plays_ranked 
  JOIN time_ranked ON (plays_ranked.row = time_ranked.row);
