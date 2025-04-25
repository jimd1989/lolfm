.mode list
SELECT printf('%,d', (SUM(duration) / (60 * 60)))
  FROM plays;
