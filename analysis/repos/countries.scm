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
  artist_plays_year AS (
    SELECT artists.country     AS country_id_year, 
           COUNT(plays.song)   AS artist_play_count_year
      FROM plays
      JOIN songs     ON plays.song      = songs.id
      JOIN artists   ON songs.artist    = artists.id
      JOIN countries ON artists.country = countries.id
     WHERE date(plays.date, 'unixepoch', 'localtime') > 
           date('now', '-12 months', 'localtime')
     GROUP BY artists.id, artists.name, artists.country, countries.id
  ),
  country_totals AS (
    SELECT country_id,
           SUM(artist_play_count) AS total_country_plays,
           SUM(artist_play_seconds) AS total_country_seconds
      FROM artist_plays
     GROUP BY country_id
  ),
  country_totals_year AS (
    SELECT country_id_year,
           SUM(artist_play_count_year) AS total_country_plays_year
      FROM artist_plays_year
     GROUP BY country_id_year
  ),
  rankings AS (
    SELECT artist_plays.country_id,
           artist_plays.country_name,
           country_totals.total_country_plays,
           artist_plays.artist_id,
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
           artist_plays.artist_id,
           artist_plays.artist_name,
           artist_plays.artist_play_seconds,
           ROW_NUMBER() OVER (
             PARTITION BY artist_plays.country_id
                 ORDER BY artist_plays.artist_play_seconds DESC
           ) AS artist_rank_time_in_country
      FROM artist_plays
      JOIN country_totals ON artist_plays.country_id = country_totals.country_id
  ),
  rankings_year AS (
    SELECT country_id_year,
           total_country_plays_year,
           ROW_NUMBER() OVER (
                 ORDER BY total_country_plays_year DESC
           ) AS country_rank_year
      FROM country_totals_year
  ),
  top AS ( 
  SELECT ROW_NUMBER() OVER (ORDER BY total_country_plays DESC) AS top_n,
         DENSE_RANK() OVER (ORDER BY total_country_plays DESC) AS country_row,
         country_id,
         total_country_plays,
         country_name,
         artist_rank_in_country,
         artist_id,
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
         artist_id,
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
         top.artist_id,
         top.artist_name,
         artist_play_count,
         country_row_time,
         top_time.country_id,
         total_country_seconds,
         top_time.country_name,
         artist_rank_time_in_country,
         top_time.artist_id,
         top_time.artist_name,
         artist_play_seconds,
         COALESCE(country_rank_year, -1),
         COALESCE(total_country_plays_year, -1)
    FROM top_time
    JOIN top  ON top.country_id         = top_time.country_id
             AND artist_rank_in_country = artist_rank_time_in_country
    LEFT JOIN rankings_year ON top.country_id = rankings_year.country_id_year
   ORDER BY country_row ASC, artist_rank_in_country ASC
   ")

(define-sql-record countries-row
  (country-rank-plays   s⊥n)
  (country-id-plays     s⊥n)
  (country-plays        s⊥n)
  (country-name-plays   s⊥s)
  (artist-rank-plays    s⊥n)
  (artist-id-plays      s⊥n) 
  (artist-name-plays    s⊥s)
  (artist-plays         s⊥n)
  (country-rank-seconds s⊥n)
  (country-id-seconds   s⊥n)
  (country-seconds      s⊥n)
  (country-name-seconds s⊥s)
  (artist-rank-seconds  s⊥n)
  (artist-id-seconds    s⊥n) 
  (artist-name-seconds  s⊥s) 
  (artist-seconds       s⊥n)
  (country-rank-year    s⊥n)
  (country-plays-year   s⊥n))

(← (get-countries db)
  (get-sql db countries-query decode-countries-row 
           (λ (acc ω) (lift2 ⊆v⊂ ω acc))
           (right (slice))))
