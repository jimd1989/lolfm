(import (chicken load))
(include-relative "../helpers/decoder.scm")
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/stream.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")

(← countries-by-plays-query "
  WITH top_plays AS (
   SELECT countries.id, countries.name, COUNT(plays.song) AS 'plays'
     FROM plays
     JOIN songs     ON plays.song      = songs.id
     JOIN artists   ON songs.artist    = artists.id
     JOIN countries ON artists.country = countries.id
    GROUP BY countries.name
    ORDER BY Plays DESC
  ),
  plays_ranked AS (
    SELECT ROW_NUMBER() OVER (ORDER BY top_plays.Plays DESC) AS 'row',
           id,
           name,
           Plays
      FROM top_plays
  )
    SELECT plays_ranked.row, plays_ranked.id, plays_ranked.Plays, 
           plays_ranked.name, artists.name, COUNT(songs.id) AS artist_play_count
      FROM artists
      JOIN countries    ON artists.country = countries.id
      JOIN plays_ranked ON countries.id    = plays_ranked.id
      JOIN songs        ON artists.id      = songs.artist
      JOIN plays        ON songs.id        = plays.song
     GROUP BY artists.id
     ORDER BY plays_ranked.row ASC, artist_play_count DESC
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

(← (countries-by-plays n db)
  (transduce stream⇒
             (∘ decode-countries-by-plays 
                (chunk-on ((C >>=) (λ (ω) (∈ 'country-id ω))))
                (transducer ((C ↑n) n)))
             ⊃ 
             ∅ 
             (stream-countries-by-plays db)))
