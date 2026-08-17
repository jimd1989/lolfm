(← plays-query "
  SELECT strftime('%Y-%m-%d %H:%M', plays.date, 'unixepoch', 'localtime'),
         artists.id,
         artists.name,
         EXISTS (SELECT 1 FROM loved WHERE loved.song = songs.id),
         songs.title
    FROM plays 
    JOIN songs   ON (plays.song = songs.id) 
    JOIN artists ON (songs.artist = artists.id) 
   ORDER BY date DESC 
")

(define-sql-record plays-row
  (date s⊥s)
  (artist-id s⊥n)
  (artist-name s⊥s)
  (loved? s⊥b)
  (title s⊥s))

(← (keep-first-page acc ω)
  (∃ ((n (↑ acc)) (α (↑↓ acc)) (l (ρ ω)))
    (? (∅? α) `(,l ,ω) `(,(+ n l) ,α))))

(← (get-plays db)
  (get-sql db plays-query decode-plays-row
           (λ (acc ω) (lift2 keep-first-page acc ω))
           (right `(0 ,∅))))
