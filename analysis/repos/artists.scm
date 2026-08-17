(← artists-query "
  WITH
  artist_plays AS (
    SELECT artists.id          AS artist_id, 
           artists.name        AS artist_name,
           COUNT(plays.song)   AS artist_play_count,
           SUM(plays.duration) AS artist_play_seconds
      FROM plays
      JOIN songs   ON plays.song   = songs.id
      JOIN artists ON songs.artist = artists.id
     GROUP BY artists.id
  ),
  artist_plays_year AS (
    SELECT artists.id        AS artist_id_year, 
           artists.name      AS artist_name_year,
           COUNT(plays.song) AS artist_play_count_year
      FROM plays
      JOIN songs     ON plays.song      = songs.id
      JOIN artists   ON songs.artist    = artists.id
     WHERE date(plays.date, 'unixepoch', 'localtime') > 
           date('now', '-12 months', 'localtime')
     GROUP BY artists.id, artists.name 
  ),
  rankings AS (
    SELECT artist_plays.artist_id AS rank_id,
           ROW_NUMBER() OVER (
             ORDER BY artist_plays.artist_play_count DESC
           ) AS artist_rank_plays,
           ROW_NUMBER() OVER (
             ORDER BY artist_plays.artist_play_seconds DESC
           ) AS artist_rank_seconds
      FROM artist_plays
  ),
  rankings_year AS (
    SELECT artist_id_year         AS year_plays_artist_id,
           artist_name_year       AS year_plays_artist_name,
           artist_play_count_year AS year_plays_count,
           ROW_NUMBER() OVER (
                 ORDER BY artist_play_count_year DESC
           ) AS year_plays_rank
      FROM artist_plays_year
  ),
  top_plays AS (
    SELECT artist_plays.artist_id         AS top_plays_artist_id,
           artist_plays.artist_name       AS top_plays_artist_name,
           artist_plays.artist_play_count AS top_plays_count,
           rankings.artist_rank_plays     AS top_plays_rank,
           DENSE_RANK() OVER (
            ORDER BY rankings.artist_rank_plays DESC
           ) AS top_plays_row
      FROM artist_plays
      JOIN rankings ON artist_plays.artist_id = rankings.rank_id
  ),
  top_seconds AS (
    SELECT artist_plays.artist_id           AS top_seconds_artist_id,
           artist_plays.artist_name         AS top_seconds_artist_name,
           artist_plays.artist_play_seconds AS top_seconds_count,
           rankings.artist_rank_seconds     AS top_seconds_rank,
           DENSE_RANK() OVER (
            ORDER BY rankings.artist_rank_seconds DESC
           ) AS top_seconds_row
      FROM artist_plays
      JOIN rankings ON artist_plays.artist_id = rankings.rank_id
  )
  SELECT top_plays_artist_id,
         top_plays_artist_name,
         top_plays_count,
         top_plays_rank,
         top_plays_row,
         top_seconds_artist_id,
         top_seconds_artist_name,
         top_seconds_count,
         top_seconds_rank,
         top_seconds_row,
         COALESCE(year_plays_artist_id, -1),
         COALESCE(year_plays_artist_name, '∅'),
         COALESCE(year_plays_count, -1),
         COALESCE(year_plays_rank, -1)
    FROM top_plays
    JOIN top_seconds ON top_plays_rank = top_seconds_rank
    LEFT JOIN rankings_year ON top_plays_rank = year_plays_rank
   ORDER BY top_plays_rank
   ")

(define-sql-record artists-row
  (top-plays-artist-id     s⊥n)
  (top-plays-artist-name   s⊥s)
  (top-plays-count         s⊥n)
  (top-plays-rank          s⊥n)
  (top-plays-row           s⊥n)
  (top-seconds-artist-id   s⊥n)
  (top-seconds-artist-name s⊥s)
  (top-seconds-count       s⊥n)
  (top-seconds-rank        s⊥n)
  (top-seconds-row         s⊥n)
  (year-plays-artist-id    s⊥n)
  (year-plays-artist-name  s⊥s)
  (year-plays-count        s⊥n)
  (year-plays-rank         s⊥n))

(← (keep-first-page acc ω)
  (∃ ((n (↑ acc)) (α (↑↓ acc)) (l (ρ ω)))
    (? (∅? α) `(,l ,ω) `(,(+ n l) ,α))))

(← (get-artists db)
  (get-sql db artists-query decode-artists-row
           (λ (acc ω) (lift2 keep-first-page acc ω))
           (right `(0 ,∅))))
