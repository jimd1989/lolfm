(← songs-query "
  WITH
    all_songs AS (
      SELECT songs.id            AS song_id,
             COUNT(plays.song)   AS song_plays,
             SUM(plays.duration) AS song_seconds,
             songs.title         AS song_title,
             artists.id          AS artist_id,
             artists.name        AS artist_name,
             EXISTS (
               SELECT 1 FROM loved WHERE loved.song = songs.id
             ) AS loved
        FROM plays
        JOIN songs  ON plays.song   = songs.id
        JOIN artists ON songs.artist = artists.id
       GROUP BY song_id
    ),
    ranked_songs_plays AS (
      SELECT song_id     AS song_id_plays,
             song_plays  AS song_plays_plays,
             song_title  AS song_title_plays,
             artist_id   AS artist_id_plays,
             artist_name AS artist_name_plays,
             loved       AS loved_plays,
             ROW_NUMBER() OVER (
               ORDER BY song_plays DESC
             ) AS song_rank_plays
        FROM all_songs
    ),
    ranked_songs_seconds AS (
      SELECT song_id       AS song_id_seconds,
             song_seconds  AS song_seconds_seconds,
             song_title    AS song_title_seconds,
             artist_id     AS artist_id_seconds,
             artist_name   AS artist_name_seconds,
             loved         AS loved_seconds,
             ROW_NUMBER() OVER (
               ORDER BY song_seconds DESC
             ) AS song_rank_seconds
        FROM all_songs
    ),
    year_songs AS (
      SELECT songs.id            AS song_id_year,
             COUNT(plays.song)   AS song_plays_year,
             songs.title         AS song_title_year,
             artists.id          AS artist_id_year,
             artists.name        AS artist_name_year,
             EXISTS (
               SELECT 1 FROM loved WHERE loved.song = songs.id
             ) AS loved_year,
             ROW_NUMBER() OVER (
               ORDER BY COUNT(plays.song) DESC
             ) AS song_rank_year
        FROM plays
        JOIN songs  ON plays.song   = songs.id
        JOIN artists ON songs.artist = artists.id
       WHERE date(plays.date, 'unixepoch', 'localtime') > 
             date('now', '-12 months', 'localtime')
       GROUP BY song_id_year
    )
    SELECT song_id_plays,
           song_plays_plays,
           song_title_plays,
           artist_id_plays,
           artist_name_plays,
           song_rank_plays,
           loved_plays,
           song_id_seconds,
           song_seconds_seconds,
           song_title_seconds,
           artist_id_seconds,
           artist_name_seconds,
           song_rank_seconds,
           loved_seconds,
           COALESCE(song_id_year, -1),
           COALESCE(song_plays_year, -1),
           COALESCE(song_title_year, '∅'),
           COALESCE(artist_id_year, -1),
           COALESCE(artist_name_year, '∅'),
           COALESCE(song_rank_year, -1),
           COALESCE(loved_year, 0)
      FROM ranked_songs_plays
      JOIN ranked_songs_seconds ON song_rank_plays   = song_rank_seconds
      LEFT JOIN year_songs      ON song_rank_seconds = song_rank_year
      ORDER BY song_rank_plays ASC
")

(define-sql-record song-row
  (song-id-plays       s⊥n)
  (plays               s⊥n)
  (song-title-plays    s⊥s)
  (artist-id-plays     s⊥n)
  (artist-name-plays   s⊥s)
  (rank-plays          s⊥n)
  (loved?-plays        s⊥b)
  (song-id-seconds     s⊥n)
  (seconds             s⊥n)
  (song-title-seconds  s⊥s)
  (artist-id-seconds   s⊥n)
  (artist-name-seconds s⊥s)
  (rank-seconds        s⊥n)
  (loved?-seconds      s⊥b)
  (song-id-year        s⊥n)
  (plays-year          s⊥n)
  (song-title-year     s⊥s)
  (artist-id-year      s⊥n)
  (artist-name-year    s⊥s)
  (rank-year           s⊥n)
  (loved?-year         s⊥b))

(← (keep-first-page acc ω)
  (∃ ((n (↑ acc)) (α (↑↓ acc)) (l (ρ ω)))
    (? (∅? α) `(,l ,ω) `(,(+ n l) ,α))))

(← (get-songs db)
  (get-sql db songs-query decode-song-row
           (λ (acc ω) (lift2 keep-first-page acc ω))
           (right `(0 ,∅))))
