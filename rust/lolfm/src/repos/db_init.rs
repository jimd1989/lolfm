use sqlite::Connection;

pub fn init_db(db: &Connection) -> Result<(), String> {
  let query = "
    PRAGMA foreign_keys = ON;

    CREATE TABLE IF NOT EXISTS raw_cmus_events(
      time          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
      status        INTEGER NOT NULL,
      artist        TEXT,
      title         TEXT,
      album         TEXT,
      album_artist  TEXT,
      genre         TEXT,
      duration      INTEGER,
      date          INTEGER
    );
    
    CREATE TABLE IF NOT EXISTS artists(
      id    INTEGER PRIMARY KEY NOT NULL,
      name  TEXT NOT NULL,
      UNIQUE(name COLLATE NOCASE)
    );
    
    CREATE TABLE IF NOT EXISTS albums(
      id            INTEGER PRIMARY KEY NOT NULL,
      title         TEXT NOT NULL,
      artist        INTEGER NOT NULL,
      year          INTEGER DEFAULT 0 NOT NULL,
      FOREIGN KEY(artist) REFERENCES artists(id),
      UNIQUE(title COLLATE NOCASE,artist)
    );
    
    CREATE TABLE IF NOT EXISTS genres(
      id    INTEGER PRIMARY KEY NOT NULL,
      name  TEXT NOT NULL,
      UNIQUE(name COLLATE NOCASE)
    );
    
    CREATE TABLE IF NOT EXISTS songs(
      id            INTEGER PRIMARY KEY NOT NULL,
      title         TEXT NOT NULL,
      artist        INTEGER NOT NULL,
      genre         INTEGER NOT NULL,
      FOREIGN KEY(artist) REFERENCES artists(id),
      FOREIGN KEY(genre) REFERENCES genres(id),
      UNIQUE(title COLLATE NOCASE,artist)
    );
    
    CREATE TABLE IF NOT EXISTS plays(
      date          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
      song          INTEGER NOT NULL,
      album         INTEGER NOT NULL,
      duration      INTEGER DEFAULT 0 NOT NULL,
      FOREIGN KEY (song) REFERENCES songs(id),
      FOREIGN KEY (album) REFERENCES albums(id),
      UNIQUE(date)
    );
    
    CREATE TABLE IF NOT EXISTS loved(
      date          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
      song          INTEGER NOT NULL,
      FOREIGN KEY(song) REFERENCES songs(id)
    );
    
    CREATE TABLE IF NOT EXISTS suppressed(
      date          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
      artist        INTEGER NOT NULL,
      FOREIGN KEY(artist) REFERENCES artists(id)
    );
    ";
    db.execute("BEGIN TRANSACTION")
      .and_then(|_| db.execute(query))
      .and_then(|_| db.execute("COMMIT"))
      .map_err(|e| e.to_string())
}
