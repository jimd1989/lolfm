(← render-genres-table
  (tabbed-table-transformer "genres"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,genre-row-rank-plays ,I)
         `("Genre" ,genre-row-genre-name-plays ,I)
         `("Plays" ,genre-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,genre-row-rank-seconds ,I)
         `("Genre" ,genre-row-genre-name-seconds ,I)
         `("Hours" ,genre-row-seconds ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,genre-row-rank-year ,I)
         `("Genre" ,genre-row-genre-name-year ,I)
         `("Plays" ,genre-row-plays-year ,n⊥s)))))

(← (render-genres paginated-rows)
  (for (← page (ι 0 paginated-rows))
       (← next (ι 1 paginated-rows))
       (← rows (ι 2 paginated-rows))
       (← table (render-genres-table rows))
       (name "Genres")
       (next-link (? next `(h3 (a (@ (href ,(◇ "./" next ".html"))) "More")) ∅))
       (contents (html name `(h1 ,name) table next-link))
       (filename (◇ page ".html"))
       (← ok? (write-html "/tmp/lolfm/genres" filename contents))
       (yield ok?)))
