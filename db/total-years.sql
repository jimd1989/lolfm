.mode list
WITH
last_play AS (
  SELECT date AS last
    FROM plays
   ORDER BY date DESC
   LIMIT 1
),
first_play AS (
  SELECT date AS first
    FROM plays
   WHERE date > 949204448
   ORDER BY date ASC
   LIMIT 1
)
SELECT printf('%,d', CAST(ceil((last - first) / (365.25 * 24 * 60 * 60)) AS INT))
  FROM (SELECT last FROM last_play),
       (SELECT first FROM first_play);
