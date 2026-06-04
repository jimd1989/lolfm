(import (chicken load))
(include-relative "../helpers/decoder.scm")
(include-relative "../helpers/stream.scm")
(include-relative "../helpers/syntax.scm")

(← query "
  WITH top_plays AS (
   SELECT countries.name, COUNT(plays.song) AS 'Plays'
     FROM plays
     JOIN songs     ON plays.song      = songs.id
     JOIN artists   ON songs.artist    = artists.id
     JOIN countries ON artists.country = countries.id
    GROUP BY countries.name
    ORDER BY Plays DESC
  ),
  plays_ranked AS (
    SELECT ROW_NUMBER() OVER (ORDER BY top_plays.Plays DESC) AS 'row',
           name,
           Plays
      FROM top_plays
  )
    SELECT plays_ranked.row AS '#',
           plays_ranked.name AS 'Country',
           plays_ranked.Plays AS 'Plays'
      FROM plays_ranked")

(← (countries-by-plays db) (stream-sql db query))
(← (decode-countries-by-plays ω)
  (decode `(,(decoder 'n s⊥n) ,(decoder 'country s⊥s) ,(decoder 'plays s⊥n)) ω))
