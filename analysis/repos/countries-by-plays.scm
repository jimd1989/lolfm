(import (chicken load))
(include-relative "../helpers/decoder.scm")
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/stream.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")

(← countries-by-plays-query "
  WITH
  artist_plays AS (
    SELECT artists.id        AS artist_id, 
           artists.name      AS artist_name,
           artists.country   AS country_id, 
           countries.name    AS country_name,
           COUNT(plays.song) AS artist_play_count
      FROM plays
      JOIN songs     ON plays.song      = songs.id
      JOIN artists   ON songs.artist    = artists.id
      JOIN countries ON artists.country = countries.id
     GROUP BY artists.id, artists.name, artists.country, countries.id
  ),
  country_totals AS (
    SELECT country_id,
           SUM(artist_play_count) AS total_country_plays
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
  )
  SELECT DENSE_RANK() OVER (ORDER BY total_country_plays DESC) AS country_row,
         country_id,
         total_country_plays,
         country_name,
         artist_name,
         artist_play_count
    FROM rankings
   WHERE artist_rank_in_country <= 50
   ORDER BY total_country_plays DESC, artist_play_count DESC
   ")

(← (stream-countries-by-plays db) (stream-sql db countries-by-plays-query))
(← decode-countries-by-plays
  (transducer (λ (ω) (decode `(,(decoder 'n s⊥n) 
                               ,(decoder 'country-id s⊥n)
                               ,(decoder 'country-plays s⊥n)
                               ,(decoder 'country s⊥s)
                               ,(decoder 'artist s⊥s)
                               ,(decoder 'artist-plays s⊥n))
                             ω))))

(← (countries-by-plays db)
  (transduce stream⇒
             (∘ decode-countries-by-plays 
                (chunk-on ((C >>=) (λ (ω) (∈ 'country-id ω)))))
             ⊃ 
             ∅ 
             (stream-countries-by-plays db)))
