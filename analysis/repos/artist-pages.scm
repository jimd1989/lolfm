(import (chicken load))
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/sorted-slices.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "common.scm")

(← artist-pages-query "
  WITH
    top_songs AS (
      SELECT artists.id         AS artist_id,
             artists.name       AS artist_name,
             songs.title        AS song_title,
             COUNT (plays.song) AS song_total_plays,
             ROW_NUMBER() OVER (
               PARTITION BY artists.id 
                   ORDER BY COUNT(plays.song) DESC
             ) AS song_rank
        FROM plays
        JOIN songs   ON plays.song   = songs.id
        JOIN artists ON songs.artist = artists.id
        GROUP BY songs.id
    ),
    top_albums AS (
      SELECT artists.id          AS artist_id,
             artists.name        AS artist_name,
             albums.title        AS album_title,
             COUNT (plays.album) AS album_total_plays,
             ROW_NUMBER() OVER (
               PARTITION BY artists.id 
                   ORDER BY COUNT(plays.album) DESC
             ) AS album_rank
        FROM plays
        JOIN albums  ON plays.album   = albums.id
        JOIN artists ON albums.artist = artists.id
       GROUP BY albums.id
    )
    SELECT COALESCE(top_songs.artist_id, top_albums.artist_id),
           COALESCE(top_songs.artist_name, top_albums.artist_name),
           COALESCE(top_songs.song_rank, -1),
           COALESCE(top_songs.song_title, '∅'),
           COALESCE(top_songs.song_total_plays, -1),
           COALESCE(top_albums.album_rank, -1),
           COALESCE(top_albums.album_title, '∅'),
           COALESCE(top_albums.album_total_plays, -1)
    FROM top_songs
    FULL OUTER JOIN top_albums
      ON top_songs.artist_id = top_albums.artist_id
     AND song_rank           = album_rank
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
  (decode-record make-artist-page-row (⊆ s⊥n s⊥s s⊥n s⊥s s⊥n s⊥s s⊥s s⊥n) ω))

(← (get-artist-pages db)
  (λ (r) (†⇒ stream⇒ 
             (∘ († decode-artist-page-row) r)
             (λ (_ ω) (⊙ (K #t) ω))
             (right #t) 
             (stream-artist-pages db))))
