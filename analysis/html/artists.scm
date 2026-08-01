(include-relative "../helpers/prelude.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/artists.scm")

(← (top-artist-link id name) (λ (ω) (link "../artist-page/" (id ω) (name ω))))
(← top-artist-link-plays 
  (top-artist-link artists-row-top-plays-artist-id
                   artists-row-top-plays-artist-name))
(← top-artist-link-seconds 
  (top-artist-link artists-row-top-seconds-artist-id
                   artists-row-top-seconds-artist-name))

(← render-top-artists-table
  (tabbed-table-transformer "artists"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,artists-row-top-plays-rank ,I)
         `("Artist" ,I ,top-artist-link-plays)
         `("Plays" ,artists-row-top-plays-count ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,artists-row-top-seconds-rank ,I)
         `("Artist" ,I ,top-artist-link-seconds)
         `("Hours" ,artists-row-top-seconds-count ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,artists-row-year-plays-rank ,I)
         `("Artist" ,artists-row-year-plays-artist-name ,I)
         `("Plays" ,artists-row-year-plays-count ,n⊥s)))))

(← (render-top-artists paginated-rows)
  (for (← page (ι 0 paginated-rows))
       (← next (ι 1 paginated-rows))
       (← rows (ι 2 paginated-rows))
       (← table (render-top-artists-table rows))
       (name "Artists")
       (next-link (? next `(h3 (a (@ (href ,(◇ "./" next ".html"))) "More")) ∅))
       (contents (html name `(h1 ,name) table next-link))
       (filename (◇ page ".html"))
       (← ok? (write-html "/tmp/lolfm/artists" filename contents))
       (yield ok?)))
