CREATE TABLE library_dump (
  duration      INTEGER NOT NULL,
  title         TEXT NOT NULL,
  album         TEXT NOT NULL,
  artist        TEXT NOT NULL,
  album_artist  TEXT NOT NULL,
  genre         TEXT NOT NULL,
  year          INTEGER NOT NULL
);

CREATE TABLE lastfm_dump (
  date          INTEGER NOT NULL,       
  artist        TEXT NOT NULL,
  title         TEXT NOT NULL,
  album         TEXT
);

CREATE TABLE librefm_dump (
  user_id       INTEGER NOT NULL,
  artist        TEXT NOT NULL,
  title         TEXT NOT NULL,
  album         TEXT NOT NULL,
  date          INTEGER NOT NULL
);

CREATE TABLE master_dump (
  duration      INTEGER NOT NULL,
  title         TEXT NOT NULL,
  album         TEXT NOT NULL,
  artist        TEXT NOT NULL,
  album_artist  TEXT NOT NULL,
  genre         TEXT NOT NULL,
  year          INTEGER NOT NULL,
  date          INTEGER NOT NULL
);
