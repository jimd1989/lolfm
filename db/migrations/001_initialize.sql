PRAGMA foreign_keys = ON;

CREATE TABLE artists(
  id    INTEGER PRIMARY KEY NOT NULL,
  name  TEXT NOT NULL UNIQUE
);

CREATE TABLE albums(
  id            INTEGER PRIMARY KEY NOT NULL,
  title         TEXT NOT NULL,
  artist        INTEGER NOT NULL,
  FOREIGN KEY(artist) REFERENCES artists(id)
);

CREATE TABLE songs(
  id            INTEGER PRIMARY KEY NOT NULL,
  title         TEXT NOT NULL,
  artist        INTEGER NOT NULL,
  FOREIGN KEY(artist) REFERENCES artists(id)
);

CREATE TABLE plays(
  date  INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
  song  INTEGER NOT NULL,
  album INTEGER NOT NULL,
  FOREIGN KEY (song) REFERENCES songs(id),
  FOREIGN KEY (album) REFERENCES albums(id)
);

CREATE TABLE loved(
  date          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
  is_loved      BOOL NOT NULL,
  song          INTEGER NOT NULL,
  FOREIGN KEY(song) REFERENCES songs(id)
);

CREATE TABLE suppressed(
  date          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
  is_suppressed BOOL NOT NULL,
  artist        INTEGER NOT NULL,
  FOREIGN KEY(artist) REFERENCES artists(id)
);
