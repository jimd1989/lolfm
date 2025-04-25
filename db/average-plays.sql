.mode list
SELECT printf('%,d', CAST(AVG(count) AS INT))
  FROM (
    SELECT COUNT(*) AS count
      FROM plays
     GROUP BY date(plays.date, 'unixepoch')
);
