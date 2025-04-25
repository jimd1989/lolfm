.mode list
SELECT printf('%,d', COUNT(album))
  FROM (
    SELECT DISTINCT plays.album
      FROM plays
);
