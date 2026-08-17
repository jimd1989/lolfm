(← discoveries-query "
  SELECT date(plays.date, 'unixepoch', 'localtime') AS date,
         artists.id                                 AS artist_id,
         artists.name                               AS artist_name,
         albums.title                               AS title
    FROM albums 
    JOIN plays   ON (albums.id     = plays.album)
    JOIN artists ON (albums.artist = artists.id)
   GROUP BY albums.id
   ORDER BY date DESC
")

(define-sql-record discoveries-row
  (date s⊥s)
  (artist-id s⊥n)
  (artist-name s⊥s)
  (title s⊥s))

(← (get-discoveries db)
  (get-sql db discoveries-query decode-discoveries-row
           (λ (acc ω) (lift2 ⊃ acc ω))
           (right ∅)))
