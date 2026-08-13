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
           EXISTS (SELECT 1 FROM loved WHERE loved.song = songs.id) AS loved,
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
         COALESCE(loved, 0),
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

(define-sql-record artist-page-row
  (artist-id   s⊥n)
  (artist-name s⊥s)
  (song-rank   s⊥n)
  (song-title  s⊥s)
  (song-plays  s⊥n)
  (loved?      s⊥b)
  (album-rank  s⊥n)
  (album-title s⊥s)
  (album-plays s⊥n))

(← (get-artist-pages db)
  (λ (r) (†⇒ stream⇒ 
             (∘ († decode-artist-page-row) r)
             (λ (_ ω) (⊙ (K #t) ω))
             (right #t) 
             (stream-artist-pages db))))
