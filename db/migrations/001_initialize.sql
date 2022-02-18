PRAGMA foreign_keys = ON;

CREATE TABLE artists(
  id    INTEGER PRIMARY KEY NOT NULL,
  name  TEXT NOT NULL UNIQUE
);

CREATE TABLE albums(
  id            INTEGER PRIMARY KEY NOT NULL,
  title         TEXT NOT NULL,
  artist        INTEGER NOT NULL,
  year          INTEGER DEFAULT 0 NOT NULL,
  FOREIGN KEY(artist) REFERENCES artists(id),
  UNIQUE(title,artist)
);

CREATE TABLE genres(
  id    INTEGER PRIMARY KEY NOT NULL,
  name  TEXT NOT NULL UNIQUE
);

CREATE TABLE songs(
  id            INTEGER PRIMARY KEY NOT NULL,
  title         TEXT NOT NULL,
  artist        INTEGER NOT NULL,
  genre         INTEGER NOT NULL,
  FOREIGN KEY(artist) REFERENCES artists(id),
  FOREIGN KEY(genre) REFERENCES genres(id),
  UNIQUE(title,artist)
);

CREATE TABLE plays(
  date          INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
  song          INTEGER NOT NULL,
  album         INTEGER NOT NULL,
  duration      INTEGER DEFAULT 0 NOT NULL,
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
