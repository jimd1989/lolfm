.mode list
SELECT ROUND(100.0 * COUNT(loved.song) / COUNT(songs.id), 2)
  FROM songs
  LEFT JOIN loved ON songs.id = loved.song;
