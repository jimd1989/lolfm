(import (chicken load))
(include-relative "../helpers/decoder.scm")
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "common.scm")

(← countries-query "
  WITH
  artist_plays AS (
    SELECT artists.id          AS artist_id, 
           artists.name        AS artist_name,
           artists.country     AS country_id, 
           countries.name      AS country_name,
           COUNT(plays.song)   AS artist_play_count,
           SUM(plays.duration) AS artist_play_seconds
      FROM plays
      JOIN songs     ON plays.song      = songs.id
      JOIN artists   ON songs.artist    = artists.id
      JOIN countries ON artists.country = countries.id
     GROUP BY artists.id, artists.name, artists.country, countries.id
  ),
  country_totals AS (
    SELECT country_id,
           SUM(artist_play_count) AS total_country_plays,
           SUM(artist_play_seconds) AS total_country_seconds
      FROM artist_plays
     GROUP BY country_id
  ),
  rankings AS (
    SELECT artist_plays.country_id,
           artist_plays.country_name,
           country_totals.total_country_plays,
           artist_plays.artist_name,
           artist_plays.artist_play_count,
           ROW_NUMBER() OVER (
             PARTITION BY artist_plays.country_id
                 ORDER BY artist_plays.artist_play_count DESC
           ) AS artist_rank_in_country
      FROM artist_plays
      JOIN country_totals ON artist_plays.country_id = country_totals.country_id
  ),
  rankings_time AS (
    SELECT artist_plays.country_id,
           artist_plays.country_name,
           country_totals.total_country_seconds,
           artist_plays.artist_name,
           artist_plays.artist_play_seconds,
           ROW_NUMBER() OVER (
             PARTITION BY artist_plays.country_id
                 ORDER BY artist_plays.artist_play_seconds DESC
           ) AS artist_rank_time_in_country
      FROM artist_plays
      JOIN country_totals ON artist_plays.country_id = country_totals.country_id
  ),
  top AS ( 
  SELECT ROW_NUMBER() OVER (ORDER BY total_country_plays DESC) AS top_n,
         DENSE_RANK() OVER (ORDER BY total_country_plays DESC) AS country_row,
         country_id,
         total_country_plays,
         country_name,
         artist_rank_in_country,
         artist_name,
         artist_play_count
    FROM rankings
   WHERE artist_rank_in_country <= 50
   ORDER BY total_country_plays DESC, artist_play_count DESC
   ),
  top_time AS ( 
  SELECT ROW_NUMBER()
                 OVER (ORDER BY total_country_seconds DESC) AS top_n_time,
         DENSE_RANK()
                 OVER (ORDER BY total_country_seconds DESC) AS country_row_time,
         country_id,
         total_country_seconds,
         country_name,
         artist_rank_time_in_country,
         artist_name,
         artist_play_seconds
    FROM rankings_time
   WHERE artist_rank_time_in_country <= 50
   ORDER BY total_country_seconds DESC, artist_play_seconds DESC
   )
  SELECT country_row,
         top.country_id,
         total_country_plays,
         top.country_name,
         artist_rank_in_country,
         top.artist_name,
         artist_play_count,
         country_row_time,
         top_time.country_id,
         total_country_seconds,
         top_time.country_name,
         artist_rank_time_in_country,
         top_time.artist_name,
         artist_play_seconds
    FROM top_time
    JOIN top ON top_n_time = top_n
   ")

(← (stream-countries db) (stream-sql db countries-query))
(← decode-countries-row
   (⊙t (λ (ω) (decode `(,(decoder 'country-rank-plays s⊥n) 
                        ,(decoder 'country-id-plays s⊥n) 
                        ,(decoder 'country-plays s⊥n) 
                        ,(decoder 'country-name-plays s⊥s) 
                        ,(decoder 'artist-rank-plays s⊥n) 
                        ,(decoder 'artist-name-plays s⊥s) 
                        ,(decoder 'artist-plays s⊥n) 
                        ,(decoder 'country-rank-seconds s⊥n) 
                        ,(decoder 'country-id-seconds s⊥n) 
                        ,(decoder 'country-seconds s⊥n) 
                        ,(decoder 'country-name-seconds s⊥s) 
                        ,(decoder 'artist-rank-seconds s⊥n) 
                        ,(decoder 'artist-name-seconds s⊥s) 
                        ,(decoder 'artist-seconds s⊥n))
                      ω))))

; entry point should be non-reified transduction, allowing further reduction r
(← (get-countries db)
  (λ (r) (transduce
           stream⇒
           (∘ decode-countries-row r)
             ⊃ 
             ∅ 
             (stream-countries db))))
