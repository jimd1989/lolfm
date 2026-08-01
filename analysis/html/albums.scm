(include-relative "../helpers/prelude.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/albums.scm")

(← (album-link id name) (λ (ω) (link "../artist-page/" (id ω) (name ω))))
(← album-link-plays 
  (album-link album-row-artist-id-plays album-row-artist-name-plays))
(← album-link-seconds 
  (album-link album-row-artist-id-seconds album-row-artist-name-seconds))
(← album-link-year
  (album-link album-row-artist-id-year album-row-artist-name-year))

(← render-albums-table
  (tabbed-table-transformer "albums"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,album-row-rank-plays ,I)
         `("Artist" ,I ,album-link-plays)
         `("Album" ,album-row-album-title-plays ,I)
         `("Plays" ,album-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,album-row-rank-seconds ,I)
         `("Artist" ,I ,album-link-seconds)
         `("Album" ,album-row-album-title-seconds ,I)
         `("Hours" ,album-row-seconds ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,album-row-rank-year ,I)
         `("Artist" ,I ,album-link-year)
         `("Album" ,album-row-album-title-year ,I)
         `("Plays" ,album-row-plays-year ,n⊥s)))))

(← (render-albums paginated-rows)
  (for (← page (ι 0 paginated-rows))
       (← next (ι 1 paginated-rows))
       (← rows (ι 2 paginated-rows))
       (← table (render-albums-table rows))
       (name "Albums")
       (next-link (? next `(h3 (a (@ (href ,(◇ "./" next ".html"))) "More")) ∅))
       (contents (html name `(h1 ,name) table next-link))
       (filename (◇ page ".html"))
       (← ok? (write-html "/tmp/lolfm/albums" filename contents))
       (yield ok?)))
