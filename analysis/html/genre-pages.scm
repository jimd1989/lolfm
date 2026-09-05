(← (genre-page-artist-link id name) 
  (λ (ω) (link "../artist-page/" (id ω) (name ω))))
(← genre-page-artist-link-plays
  (genre-page-artist-link genre-pages-row-artist-id-plays
                          genre-pages-row-artist-name-plays))
(← genre-page-artist-link-seconds
  (genre-page-artist-link genre-pages-row-artist-id-seconds
                          genre-pages-row-artist-name-seconds))

(← render-genre-page-table
  (tabbed-table-transformer "genre-pages"
    `("Plays"
       ,(table-transformer 'rank
         `("#" ,genre-pages-row-rank ,I)
         `("Artist" ,I ,genre-page-artist-link-plays)
         `("Plays" ,genre-pages-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'rank
         `("#" ,genre-pages-row-rank ,I)
         `("Artist" ,I ,genre-page-artist-link-seconds)
         `("Hours" ,genre-pages-row-seconds ,seconds⊥hours)))))

(← (render-genre-pages rows)
  (for (head (⊆vι 0 rows))
       (name (◇ " " (genre-pages-row-genre-name head)))
       (id (genre-pages-row-genre-id head))
       (← table (render-genre-page-table rows))
       (contents (html name `(h1 ,name) table))
       (filename (◇ id ".html"))
       (← ok? (write-html "/tmp/lolfm/genre-page" filename contents))
       (yield ok?)))
