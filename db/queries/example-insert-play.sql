BEGIN TRANSACTION;
  INSERT INTO artists(name) VALUES('大友良英');        -- artist 
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO artists(name) VALUES('Various Artists'); -- albumartist
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  INSERT INTO genres(name) VALUES('Experimental');
  ON CONFLICT(name) DO UPDATE SET name=excluded.name;

  WITH album_artist AS (SELECT id FROM artists WHERE name='Various Artists')
  INSERT INTO albums(title, artist, year)
  VALUES('Improvised Music from Japan', (SELECT id FROM album_artist), 2001)
  ON CONFLICT(title, artist) DO UPDATE SET year=excluded.year;

  WITH artist AS (SELECT id FROM artists WHERE name='大友良英'),
       genre AS (SELECT id FROM genres WHERE name='Experimental')
  INSERT INTO songs(title, artist, genre)
  VALUES('Cathode #4: Sound Check Version',
         (SELECT id FROM artist),
         (SELECT id FROM genre))
  ON CONFLICT(title, artist) DO UPDATE SET genre=excluded.genre;

  WITH song AS (
         SELECT id FROM songs 
         WHERE artist=(SELECT id FROM artists WHERE name='大友良英')
         AND   title=('Cathode #4: Sound Check Version')),
       album AS (
          SELECT id FROM albums
          WHERE artist=(SELECT id FROM artists WHERE name='Various Artists')
          AND   title=('Improvised Music from Japan'))
   INSERT INTO plays(song, album, duration)
   VALUES((SELECT id FROM song), (SELECT id FROM album), 325);
COMMIT;
