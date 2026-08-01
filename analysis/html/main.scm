(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/sorted-slices.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/albums.scm")
(include-relative "../repos/artists.scm")
(include-relative "../repos/artist-pages.scm")
(include-relative "../repos/countries.scm")

(← (render-main-head)
  `((h1 "lol.fm")
    (p "lolfm is an industry leading amazingly simple scrobbler (ASS). " 
       "Just cmus and a local sqlite file on your hard drive. " 
       "If you'd like to run it yourself, check it out on "
       (a (@ (href "https://github.com/jimd1989/lolfm")) "Github")
       ".")))

(← (main-artist-page-link id name) 
  (λ (ω) (link "./artist-page/" (id ω) (name ω))))

(← main-artist-page-link-plays
  (main-artist-page-link artists-row-top-plays-artist-id 
                         artists-row-top-plays-artist-name))

(← main-artist-page-link-seconds
  (main-artist-page-link artists-row-top-seconds-artist-id 
                         artists-row-top-seconds-artist-name))

(← main-artist-page-link-year-plays
  (main-artist-page-link artists-row-year-plays-artist-id 
                         artists-row-year-plays-artist-name))


(← render-main-top-artists-table
  (tabbed-table-transformer "top-artists"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,artists-row-top-plays-rank ,I)
         `("Artist" ,I ,main-artist-page-link-plays)
         `("Plays" ,artists-row-top-plays-count ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,artists-row-top-seconds-rank ,I)
         `("Artist" ,I ,main-artist-page-link-seconds)
         `("Hours" ,artists-row-top-seconds-count ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,artists-row-year-plays-rank ,I)
         `("Artist" ,I ,main-artist-page-link-year-plays)
         `("Plays" ,artists-row-year-plays-count ,n⊥s)))))

(← (render-main-top-artists artists)
  (for (title '(h2 "Artists"))
       (← count (ι 0 artists))
       (← top-artists (ι 1 artists))
       (← table (render-main-top-artists-table (↑n 50 top-artists)))
       (desc `(p ,(n⊥s count) " artists played."))
       (more '(h3 (a (@ (href "./artists/1.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← (main-album-link id name) (λ (ω) (link "./artist-page/" (id ω) (name ω))))
(← main-album-link-plays 
  (main-album-link album-row-artist-id-plays album-row-artist-name-plays))
(← main-album-link-seconds 
  (main-album-link album-row-artist-id-seconds album-row-artist-name-seconds))
(← main-album-link-year
  (main-album-link album-row-artist-id-year album-row-artist-name-year))

(← render-main-albums-table
  (tabbed-table-transformer "top-albums"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,album-row-rank-plays ,I)
         `("Artist" ,I ,main-album-link-plays)
         `("Album" ,album-row-album-title-plays ,I)
         `("Plays" ,album-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,album-row-rank-seconds ,I)
         `("Artist" ,I ,main-album-link-seconds)
         `("Album" ,album-row-album-title-seconds ,I)
         `("Hours" ,album-row-seconds ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,album-row-rank-year ,I)
         `("Artist" ,I ,main-album-link-year)
         `("Album" ,album-row-album-title-year ,I)
         `("Plays" ,album-row-plays-year ,n⊥s)))))

(← (render-main-albums albums)
  (for (title '(h2 "Albums"))
       (← count (ι 0 albums))
       (← top-albums (ι 1 albums))
       (← table (render-main-albums-table (↑n 15 top-albums)))
       (desc `(p ,(n⊥s count) " albums played."))
       (more '(h3 (a (@ (href "./albums/1.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← (main-country-link id name) (λ (ω) (link "./countries/" (id ω) (name ω))))
(← main-country-link-plays 
  (main-country-link countries-row-country-id-plays 
                    countries-row-country-name-plays))
(← main-country-link-seconds
  (main-country-link countries-row-country-id-seconds 
                    countries-row-country-name-seconds))

(← render-main-country-table
  (tabbed-table-transformer "main-countries"
    `("Plays"
       ,(table-transformer-truncated 15 'plays
         `("#" ,countries-row-country-rank-plays ,I)
         `("Country" ,I ,main-country-link-plays)
         `("Plays" ,countries-row-country-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer-truncated 15 'seconds
         `("#" ,countries-row-country-rank-seconds ,I)
         `("Country" ,I ,main-country-link-seconds)
         `("Hours" ,countries-row-country-seconds ,seconds⊥hours)))
    `("Year"
      ,(table-transformer-truncated 15 'year
         `("#" ,countries-row-country-rank-year ,I)
         `("Country" ,I ,main-country-link-plays)
         `("Plays" ,countries-row-country-plays-year ,n⊥s)))))

(← (render-main-countries countries)
  (for (title '(h2 "Countries"))
       (desc `(p ,(n⊥s (⊆vρ countries)) " countries explored."))
       (← table (render-main-country-table countries))
       (more '(h3 (a (@ (href "./countries/countries.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← (render-main artists albums countries)
  (for (name "lol.fm")
       (head (render-main-head))
       (← artists-html (render-main-top-artists artists))
       (← albums-html (render-main-albums albums))
       (← countries-html (render-main-countries countries))
       (contents ($ html `(,name ,@head ,artists-html ,albums-html 
                                 ,countries-html)))
       (filename "lolfm.html")
       (← ok? (write-html "/tmp/lolfm" filename contents))
       (yield ok?)))
