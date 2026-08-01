(import (chicken load))
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "common.scm")

(← years-query "
  WITH
    all_years AS (
      SELECT albums.year         AS year,
             COUNT(plays.date)   AS year_plays,
             SUM(plays.duration) AS year_seconds
        FROM plays
        JOIN albums ON plays.album = albums.id
       GROUP BY year
    ),
    ranked_years_plays AS (
      SELECT year       AS year_plays,
             year_plays AS year_plays_plays,
             ROW_NUMBER() OVER (
               ORDER BY year_plays DESC
             ) AS year_rank_plays
        FROM all_years
    ),
    ranked_years_seconds AS (
      SELECT year         AS year_seconds,
             year_seconds AS year_seconds_seconds,
             ROW_NUMBER() OVER (
               ORDER BY year_seconds DESC
             ) AS year_rank_seconds
        FROM all_years
    ),
    year_years AS (
      SELECT albums.year         AS year_year,
             COUNT(plays.date)   AS year_plays_year,
             ROW_NUMBER() OVER (
               ORDER BY COUNT(plays.date) DESC
             ) AS year_rank_year
        FROM plays
        JOIN albums ON plays.album = albums.id
       WHERE date(plays.date, 'unixepoch', 'localtime') > 
             date('now', '-12 months', 'localtime')
       GROUP BY year_year
    )
    SELECT year_plays,
           year_plays_plays,
           year_rank_plays,
           year_seconds,
           year_seconds_seconds,
           year_rank_seconds,
           COALESCE(year_year, -1),
           COALESCE(year_plays_year, -1),
           COALESCE(year_rank_year, -1)
      FROM ranked_years_plays
      JOIN ranked_years_seconds ON year_rank_plays   = year_rank_seconds
      LEFT JOIN year_years      ON year_rank_seconds = year_rank_year
      ORDER BY year_rank_plays ASC
")

(← (stream-years db) (stream-sql db years-query))

(define-sql-record year-row
  (year-plays   s⊥n)
  (plays        s⊥n)
  (rank-plays   s⊥n)
  (year-seconds s⊥n)
  (seconds      s⊥n)
  (rank-seconds s⊥n)
  (year-year    s⊥n)
  (plays-year   s⊥n)
  (rank-year    s⊥n))

(← (keep-first-page acc ω)
  (∃ ((n (↑ acc)) (α (↑↓ acc)) (l (ρ ω)))
    (? (∅? α) `(,l ,ω) `(,(+ n l) ,α))))

(← (get-years db)
  (λ (r) (†⇒ stream⇒ 
             (∘ († decode-year-row) r)
             (λ (acc ω) (lift2 keep-first-page acc ω))
             (right `(0 ,∅)) 
             (stream-years db))))
