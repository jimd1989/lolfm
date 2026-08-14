(← genres-query "
  WITH
    all_genres AS (
      SELECT genres.id           AS genre_id,
             genres.name         AS genre_name,
             COUNT(plays.date)   AS genre_plays,
             SUM(plays.duration) AS genre_seconds
        FROM plays
        JOIN songs  ON plays.song  = songs.id
        JOIN genres ON songs.genre = genres.id
       GROUP BY genre_id
    ),
    ranked_genres_plays AS (
      SELECT genre_id    AS genre_id_plays,
             genre_name  AS genre_name_plays,
             genre_plays AS genre_plays_plays,
             ROW_NUMBER() OVER (
               ORDER BY genre_plays DESC
             ) AS genre_rank_plays
        FROM all_genres
    ),
    ranked_genres_seconds AS (
      SELECT genre_id      AS genre_id_seconds,
             genre_name    AS genre_name_seconds,
             genre_seconds AS genre_seconds_seconds,
             ROW_NUMBER() OVER (
               ORDER BY genre_seconds DESC
             ) AS genre_rank_seconds
        FROM all_genres
    ),
    year_genres AS (
      SELECT genres.id          AS genre_id_year,
             genres.name        AS genre_name_year,
             COUNT(plays.date) AS genre_plays_year,
             ROW_NUMBER() OVER (
               ORDER BY COUNT(plays.date) DESC
             ) AS genre_rank_year
        FROM plays
        JOIN songs  ON plays.song  = songs.id
        JOIN genres ON songs.genre = genres.id
       WHERE date(plays.date, 'unixepoch', 'localtime') > 
             date('now', '-12 months', 'localtime')
       GROUP BY genre_id_year
    )
    SELECT genre_id_plays,
           genre_plays_plays,
           genre_name_plays,
           genre_rank_plays,
           genre_id_seconds,
           genre_seconds_seconds,
           genre_name_seconds,
           genre_rank_seconds,
           COALESCE(genre_id_year, -1),
           COALESCE(genre_plays_year, -1),
           COALESCE(genre_name_year, '∅'),
           COALESCE(genre_rank_year, -1)
      FROM ranked_genres_plays
      JOIN ranked_genres_seconds ON genre_rank_plays   = genre_rank_seconds
      LEFT JOIN year_genres      ON genre_rank_seconds = genre_rank_year
      ORDER BY genre_rank_plays ASC
")

(define-sql-record genre-row
  (genre-id-plays     s⊥n)
  (plays              s⊥n)
  (genre-name-plays   s⊥s)
  (rank-plays         s⊥n)
  (genre-id-seconds   s⊥n)
  (seconds            s⊥n)
  (genre-name-seconds s⊥s)
  (rank-seconds       s⊥n)
  (genre-id-year      s⊥n)
  (plays-year         s⊥n)
  (genre-name-year    s⊥s)
  (rank-year          s⊥n))

(← (keep-first-page acc ω)
  (∃ ((n (↑ acc)) (α (↑↓ acc)) (l (ρ ω)))
    (? (∅? α) `(,l ,ω) `(,(+ n l) ,α))))

(← (get-genres db)
  (get-sql db genres-query decode-genre-row
           (λ (acc ω) (lift2 keep-first-page acc ω))
           (right `(0 ,∅))))
