.headers on
.mode html
SELECT ROW_NUMBER() OVER (ORDER BY date) AS '#',
datetime(plays.date, 'unixepoch', 'localtime') AS 'Date', 
artists.name AS 'Artist', 
IIF(EXISTS (SELECT 1 FROM loved WHERE loved.song = songs.id), '♥', '') AS '♥',
songs.title AS 'Song'
FROM plays 
JOIN songs ON (plays.song = songs.id) 
JOIN artists ON (songs.artist = artists.id) 
ORDER BY date DESC 
LIMIT 15;
