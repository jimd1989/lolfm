(import (chicken load))
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "common.scm")

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

(← (stream-plays db) (stream-sql db plays-query))

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
  (λ (r) (†⇒ stream⇒ 
             (∘ († decode-plays-row) r)
             (λ (acc ω) (lift2 keep-first-page acc ω))
             (right `(0 ,∅)) 
             (stream-plays db))))
