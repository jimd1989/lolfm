(← (plays-artist-page-link row)
  (link "../artist-page/" (plays-row-artist-id row)
                          (plays-row-artist-name row)))

(← render-plays-table
  (tabbed-table-transformer "plays"
    `("Plays"
       ,(table-transformer 'plays
         `("Date" ,plays-row-date ,I)
         `("Artist" ,I ,plays-artist-page-link)
         `(,(loved #t) ,plays-row-loved? ,loved)
         `("Song" ,plays-row-title ,I)))))

(← (render-plays paginated-rows)
  (for (← page (ι 0 paginated-rows))
       (← next (ι 1 paginated-rows))
       (← rows (ι 2 paginated-rows))
       (← table (render-plays-table rows))
       (name "Plays")
       (next-link (? next `(h3 (a (@ (href ,(◇ "./" next ".html"))) "More")) ∅))
       (contents (html name `(h1 ,name) table next-link))
       (filename (◇ page ".html"))
       (← ok? (write-html "/tmp/lolfm/plays" filename contents))
       (yield ok?)))
