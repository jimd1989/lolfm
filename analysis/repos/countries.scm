(import (chicken load))
(include-relative "../helpers/monad.scm")
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
    JOIN top  ON top.country_id         = top_time.country_id
             AND artist_rank_in_country = artist_rank_time_in_country
   ORDER BY country_row ASC, artist_rank_in_country ASC
   ")

(← (stream-countries db) (stream-sql db countries-query))

(define-record-type countries-row
  (make-countries-row country-rank-plays country-id-plays country-plays 
                      country-name-plays artist-rank-plays artist-name-plays
                      artist-plays country-rank-seconds country-id-seconds
                      country-seconds country-name-seconds artist-rank-seconds
                      artist-name-seconds artist-seconds)
  countries-row?
  (country-rank-plays   countries-row-country-rank-plays)
  (country-id-plays     countries-row-country-id-plays)
  (country-plays        countries-row-country-plays)
  (country-name-plays   countries-row-country-name-plays)
  (artist-rank-plays    countries-row-artist-rank-plays)
  (artist-name-plays    countries-row-artist-name-plays)
  (artist-plays         countries-row-artist-plays)
  (country-rank-seconds countries-row-country-rank-seconds)
  (country-id-seconds   countries-row-country-id-seconds)
  (country-seconds      countries-row-country-seconds)
  (country-name-seconds countries-row-country-name-seconds)
  (artist-rank-seconds  countries-row-artist-rank-seconds)
  (artist-name-seconds  countries-row-artist-name-seconds) 
  (artist-seconds       countries-row-artist-seconds))

(← (decode-countries-row ω)
  (decode-record make-countries-row
    (⊆ s⊥n s⊥n s⊥n s⊥s s⊥n s⊥s s⊥n s⊥n s⊥n s⊥n s⊥s s⊥n s⊥s s⊥n) ω))

(← (get-countries db)
  (λ (r) (†⇒ stream⇒ 
             (∘ († decode-countries-row) r)
             (λ (acc ω) (lift2 ⊃ acc ω)) 
             (right ∅) 
             (stream-countries db))))
