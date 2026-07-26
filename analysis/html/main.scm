(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/sorted-slices.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/artists.scm")
(include-relative "../repos/countries.scm")

(← (render-main-head)
  `((h1 "lol.fm")
    (p "lolfm is an industry leading amazingly simple scrobbler (ASS). " 
       "Just cmus and a local sqlite file on your hard drive. " 
       "If you'd like to run it yourself, check it out on "
       (a (@ (href "https://github.com/jimd1989/lolfm")) "Github")
       ".")))

(← render-main-top-artists-table
  (tabbed-table-transformer "top-artists"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,artists-row-top-plays-rank ,I)
         `("Artist" ,artists-row-top-plays-artist-name ,I)
         `("Plays" ,artists-row-top-plays-count ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,artists-row-top-seconds-rank ,I)
         `("Artist" ,artists-row-top-seconds-artist-name ,I)
         `("Hours" ,artists-row-top-seconds-count ,seconds⊥hours)))))

(← (render-main-top-artists artists)
  (for (title '(h1 "Artists"))
       (← count (ι 0 artists))
       (← top-artists (ι 1 artists))
       (← table (render-main-top-artists-table (↑n 50 top-artists)))
       (desc `(p ,(n⊥s count) " artists played."))
       (more '(h3 (a (@ (href "./artists/1.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← render-main-country-table
  (tabbed-table-transformer "main-countries"
    `("Plays"
       ,(table-transformer-truncated 15 'plays
         `("#" ,countries-row-country-rank-plays ,I)
         `("Country" ,countries-row-country-name-plays ,I)
         `("Plays" ,countries-row-country-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer-truncated 15 'seconds
         `("#" ,countries-row-country-rank-seconds ,I)
         `("Country" ,countries-row-country-name-seconds ,I)
         `("Hours" ,countries-row-country-seconds ,seconds⊥hours)))
    `("Year"
      ,(table-transformer-truncated 15 'year
         `("#" ,countries-row-country-rank-year ,I)
         `("Country" ,countries-row-country-name-plays ,I)
         `("Plays" ,countries-row-country-plays-year ,n⊥s)))))

(← (render-main-countries countries)
  (for (title '(h1 "Countries"))
       (desc `(p ,(n⊥s (⊆vρ countries)) " countries explored."))
       (← table (render-main-country-table countries))
       (more '(h3 (a (@ (href "./countries/countries.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← (render-main artists countries)
  (for (name "lol.fm")
       (head (render-main-head))
       (← artists-html (render-main-top-artists artists))
       (← countries-html (render-main-countries countries))
       (contents ($ html `(,name ,@head ,artists-html ,countries-html)))
       (filename "lolfm.html")
       (← ok? (write-html "/tmp/lolfm" filename contents))
       (yield ok?)))
