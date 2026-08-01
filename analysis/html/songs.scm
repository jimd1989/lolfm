(include-relative "../helpers/prelude.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/songs.scm")

(← (song-link id name) (λ (ω) (link "../artist-page/" (id ω) (name ω))))
(← song-link-plays 
  (song-link song-row-artist-id-plays song-row-artist-name-plays))
(← song-link-seconds 
  (song-link song-row-artist-id-seconds song-row-artist-name-seconds))
(← song-link-year
  (song-link song-row-artist-id-year song-row-artist-name-year))

(← render-songs-table
  (tabbed-table-transformer "songs"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,song-row-rank-plays ,I)
         `("Artist" ,I ,song-link-plays)
         `("Song" ,song-row-song-title-plays ,I)
         `(,(loved #t) ,song-row-loved?-plays ,loved)
         `("Plays" ,song-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,song-row-rank-seconds ,I)
         `("Artist" ,I ,song-link-seconds)
         `("Song" ,song-row-song-title-seconds ,I)
         `(,(loved #t) ,song-row-loved?-seconds ,loved)
         `("Hours" ,song-row-seconds ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,song-row-rank-year ,I)
         `("Artist" ,I ,song-link-year)
         `("Song" ,song-row-song-title-year ,I)
         `(,(loved #t) ,song-row-loved?-year ,loved)
         `("Plays" ,song-row-plays-year ,n⊥s)))))

(← (render-songs paginated-rows)
  (for (← page (ι 0 paginated-rows))
       (← next (ι 1 paginated-rows))
       (← rows (ι 2 paginated-rows))
       (← table (render-songs-table rows))
       (name "Songs")
       (next-link (? next `(h3 (a (@ (href ,(◇ "./" next ".html"))) "More")) ∅))
       (contents (html name `(h1 ,name) table next-link))
       (filename (◇ page ".html"))
       (← ok? (write-html "/tmp/lolfm/songs" filename contents))
       (yield ok?)))
