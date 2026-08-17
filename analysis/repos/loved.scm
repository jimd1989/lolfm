(← loved-query "
  SELECT date(loved.date, 'unixepoch', 'localtime') AS loved, 
         artists.id                                 AS artist_id,
         artists.name                               AS artist_name, 
         songs.title                                AS title 
    FROM loved 
    JOIN songs   ON (loved.song   = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   ORDER BY Loved DESC 
")

(define-sql-record loved-row
  (date s⊥s)
  (artist-id s⊥n)
  (artist-name s⊥s)
  (title s⊥s))

(← (get-loved db)
  (get-sql db loved-query decode-loved-row
           (λ (acc ω) (lift2 ⊃ acc ω))
           (right ∅)))
