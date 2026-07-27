(import (chicken load))
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "common.scm")

(← artist-pages-query "
WITH 
  all_artists AS (
    SELECT DISTINCT artists.id AS artist_id, 
                    artists.name AS artist_name
     FROM plays
     JOIN songs   ON plays.song = songs.id
     JOIN artists ON songs.artist = artists.id
  ),
  ranked_songs AS (
    SELECT artists.id        AS artist_id,
           songs.title       AS song_title,
           COUNT(plays.song) AS song_total_plays,
           ROW_NUMBER() OVER (
             PARTITION BY artists.id ORDER BY COUNT(plays.song) DESC
           ) AS song_rank
     FROM plays
     JOIN songs   ON plays.song   = songs.id
     JOIN artists ON songs.artist = artists.id
    GROUP BY artists.id, songs.id
  ),
  ranked_albums AS (
    SELECT artists.id         AS artist_id,
           albums.title       AS album_title,
           COUNT(plays.album) AS album_total_plays,
           ROW_NUMBER() OVER (
             PARTITION BY artists.id ORDER BY COUNT(plays.album) DESC
           ) AS album_rank
     FROM plays
     JOIN albums  ON plays.album   = albums.id
     JOIN artists ON albums.artist = artists.id
    GROUP BY artists.id, albums.id
  ),
  sequence AS (
    SELECT DISTINCT artist_id, song_rank  AS rank FROM ranked_songs
    UNION
    SELECT DISTINCT artist_id, album_rank AS rank FROM ranked_albums
  )
  SELECT all_artists.artist_id,
         all_artists.artist_name,
         COALESCE(song_rank, -1),
         COALESCE(song_title, '∅'),
         COALESCE(song_total_plays, -1),
         COALESCE(album_rank, -1),
         COALESCE(album_title, '∅'),
         COALESCE(album_total_plays, -1)
    FROM sequence
    JOIN all_artists         ON sequence.artist_id       = all_artists.artist_id
    LEFT JOIN ranked_songs   ON ranked_songs.artist_id   = sequence.artist_id 
                            AND song_rank                = sequence.rank
    LEFT JOIN ranked_albums  ON ranked_albums.artist_id  = sequence.artist_id 
                            AND ranked_albums.album_rank = sequence.rank
")

(← (stream-artist-pages db) (stream-sql db artist-pages-query))

(define-record-type artist-page-row
  (make-artist-page-row artist-id artist-name song-rank song-title song-plays
                        album-rank album-title album-plays)
  artist-page-row?
  (artist-id   artist-page-row-artist-id)
  (artist-name artist-page-row-artist-name)
  (song-rank   artist-page-row-song-rank)
  (song-title  artist-page-row-song-title)
  (song-plays  artist-page-row-song-plays)
  (album-rank  artist-page-row-album-rank)
  (album-title artist-page-row-album-title)
  (album-plays artist-page-row-album-plays)
)

(← (decode-artist-page-row ω)
  (decode-record make-artist-page-row (⊆ s⊥n s⊥s s⊥n s⊥s s⊥n s⊥n s⊥s s⊥n) ω))

(← (get-artist-pages db)
  (λ (r) (†⇒ stream⇒ 
             (∘ († decode-artist-page-row) r)
             (λ (_ ω) (⊙ (K #t) ω))
             (right #t) 
             (stream-artist-pages db))))
