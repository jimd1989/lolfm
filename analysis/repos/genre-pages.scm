(← genre-pages-query "
  WITH 
  genre_plays AS (
    SELECT artists.id          AS artist_id,
           genres.id           AS genre_id,
           COUNT(plays.date)   AS genre_plays,
           SUM(plays.duration) AS genre_seconds
      FROM plays
      JOIN songs   ON plays.song   = songs.id
      JOIN genres  ON songs.genre  = genres.id
      JOIN artists ON songs.artist = artists.id
     GROUP BY artists.id, genres.id
  ),
  rankings AS (
    SELECT ROW_NUMBER() OVER (
             PARTITION BY genre_id
                 ORDER BY genre_plays DESC
           ) AS genre_rank_plays,
           ROW_NUMBER() OVER (
             PARTITION BY genre_id
                 ORDER BY genre_seconds DESC
           ) AS genre_rank_seconds,
           artist_id,
           genre_id,
           genre_plays,
           genre_seconds
      FROM genre_plays
  ),
  top_plays AS (
    SELECT artist_id,
           artists.name AS artist_name,
           genre_id,
           genres.name  AS genre_name,
           genre_rank_plays,
           genre_plays
      FROM rankings
      JOIN artists ON artist_id = artists.id
      JOIN genres  ON genre_id  = genres.id
     WHERE genre_rank_plays <= 50
     ORDER BY genre_id
  ),
  top_seconds AS (
    SELECT artist_id,
           artists.name AS artist_name,
           genre_id,
           genre_rank_seconds,
           genre_seconds
      FROM rankings
      JOIN artists ON artist_id = artists.id
     WHERE genre_rank_seconds <= 50
     ORDER BY genre_id
  )
  SELECT top_plays.genre_id,
         top_plays.genre_name,
         top_plays.genre_rank_plays,
         top_plays.artist_id,
         top_plays.artist_name,
         top_plays.genre_plays,
         top_seconds.artist_id,
         top_seconds.artist_name,
         top_seconds.genre_seconds
    FROM top_plays
    JOIN top_seconds  ON (top_plays.genre_id = top_seconds.genre_id)
                     AND (genre_rank_plays   = genre_rank_seconds)
")

(define-sql-record genre-pages-row
  (genre-id s⊥n)
  (genre-name s⊥s)
  (rank s⊥n)
  (artist-id-plays s⊥n)
  (artist-name-plays s⊥s)
  (plays s⊥n)
  (artist-id-seconds s⊥n)
  (artist-name-seconds s⊥s)
  (seconds s⊥n))

(← (get-genre-pages db)
  (get-sql db genre-pages-query decode-genre-pages-row
           (λ (_ ω) (⊙ (K #t) ω)) 
           (right #t)))
