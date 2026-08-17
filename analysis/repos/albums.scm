(← albums-query "
  WITH
    all_albums AS (
      SELECT albums.id           AS album_id,
             COUNT(plays.album)  AS album_plays,
             SUM(plays.duration) AS album_seconds,
             albums.title        AS album_title,
             artists.id          AS artist_id,
             artists.name        AS artist_name
        FROM plays
        JOIN albums  ON plays.album   = albums.id
        JOIN artists ON albums.artist = artists.id
       GROUP BY album_id
    ),
    ranked_albums_plays AS (
      SELECT album_id    AS album_id_plays,
             album_plays AS album_plays_plays,
             album_title AS album_title_plays,
             artist_id   AS artist_id_plays,
             artist_name AS artist_name_plays,
             ROW_NUMBER() OVER (
               ORDER BY album_plays DESC
             ) AS album_rank_plays
        FROM all_albums
    ),
    ranked_albums_seconds AS (
      SELECT album_id      AS album_id_seconds,
             album_seconds AS album_seconds_seconds,
             album_title   AS album_title_seconds,
             artist_id     AS artist_id_seconds,
             artist_name   AS artist_name_seconds,
             ROW_NUMBER() OVER (
               ORDER BY album_seconds DESC
             ) AS album_rank_seconds
        FROM all_albums
    ),
    year_albums AS (
      SELECT albums.id           AS album_id_year,
             COUNT(plays.album)  AS album_plays_year,
             albums.title        AS album_title_year,
             artists.id          AS artist_id_year,
             artists.name        AS artist_name_year,
             ROW_NUMBER() OVER (
               ORDER BY COUNT(plays.album) DESC
             ) AS album_rank_year
        FROM plays
        JOIN albums  ON plays.album   = albums.id
        JOIN artists ON albums.artist = artists.id
       WHERE date(plays.date, 'unixepoch', 'localtime') > 
             date('now', '-12 months', 'localtime')
       GROUP BY album_id_year
    )
    SELECT album_id_plays,
           album_plays_plays,
           album_title_plays,
           artist_id_plays,
           artist_name_plays,
           album_rank_plays,
           album_id_seconds,
           album_seconds_seconds,
           album_title_seconds,
           artist_id_seconds,
           artist_name_seconds,
           album_rank_seconds,
           COALESCE(album_id_year, -1),
           COALESCE(album_plays_year, -1),
           COALESCE(album_title_year, '∅'),
           COALESCE(artist_id_year, -1),
           COALESCE(artist_name_year, '∅'),
           COALESCE(album_rank_year, -1)
      FROM ranked_albums_plays
      JOIN ranked_albums_seconds ON album_rank_plays   = album_rank_seconds
      LEFT JOIN year_albums      ON album_rank_seconds = album_rank_year
      ORDER BY album_rank_plays ASC
")

(define-sql-record album-row
  (album-id-plays      s⊥n)
  (plays               s⊥n)
  (album-title-plays   s⊥s)
  (artist-id-plays     s⊥n)
  (artist-name-plays   s⊥s)
  (rank-plays          s⊥n)
  (album-id-seconds    s⊥n)
  (seconds             s⊥n)
  (album-title-seconds s⊥s)
  (artist-id-seconds   s⊥n)
  (artist-name-seconds s⊥s)
  (rank-seconds        s⊥n)
  (album-id-year       s⊥n)
  (plays-year          s⊥n)
  (album-title-year    s⊥s)
  (artist-id-year      s⊥n)
  (artist-name-year    s⊥s)
  (rank-year           s⊥n))

(← (keep-first-page acc ω)
  (∃ ((n (↑ acc)) (α (↑↓ acc)) (l (ρ ω)))
    (? (∅? α) `(,l ,ω) `(,(+ n l) ,α))))

(← (get-albums db)
  (get-sql db albums-query decode-album-row 
           (λ (acc ω) (lift2 keep-first-page acc ω)) (right `(0 ,∅))))
