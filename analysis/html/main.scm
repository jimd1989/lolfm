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

(← (main-song-link id name) (λ (ω) (link "./artist-page/" (id ω) (name ω))))
(← main-song-link-plays 
  (main-song-link song-row-artist-id-plays song-row-artist-name-plays))
(← main-song-link-seconds 
  (main-song-link song-row-artist-id-seconds song-row-artist-name-seconds))
(← main-song-link-year
  (main-song-link song-row-artist-id-year song-row-artist-name-year))

(← render-main-songs-table
  (tabbed-table-transformer "top-songs"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,song-row-rank-plays ,I)
         `("Artist" ,I ,main-song-link-plays)
         `("Song" ,song-row-song-title-plays ,I)
         `(,(loved #t) ,song-row-loved?-plays ,loved)
         `("Plays" ,song-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,song-row-rank-seconds ,I)
         `("Artist" ,I ,main-song-link-seconds)
         `("Song" ,song-row-song-title-seconds ,I)
         `(,(loved #t) ,song-row-loved?-seconds ,loved)
         `("Hours" ,song-row-seconds ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,song-row-rank-year ,I)
         `("Artist" ,I ,main-song-link-year)
         `("Song" ,song-row-song-title-year ,I)
         `(,(loved #t) ,song-row-loved?-year ,loved)
         `("Plays" ,song-row-plays-year ,n⊥s)))))

(← (render-main-songs songs)
  (for (title '(h2 "Songs"))
       (← count (ι 0 songs))
       (← top-songs (ι 1 songs))
       (← table (render-main-songs-table (↑n 15 top-songs)))
       (desc `(p ,(n⊥s count) " different songs played."))
       (more '(h3 (a (@ (href "./songs/1.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← render-main-genres-table
  (tabbed-table-transformer "top-genres"
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

(← (render-main-genres genres)
  (for (title '(h2 "Genres"))
       (← count (ι 0 genres))
       (← top-genres (ι 1 genres))
       (← table (render-main-genres-table (↑n 15 top-genres)))
       (desc `(p ,(n⊥s count) " genres explored."))
       (more '(h3 (a (@ (href "./genres/1.html")) "More")))
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

(← render-main-years-table
  (tabbed-table-transformer "top-years"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,year-row-rank-plays ,I)
         `("Year" ,year-row-year-plays ,I)
         `("Plays" ,year-row-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,year-row-rank-seconds ,I)
         `("Year" ,year-row-year-seconds ,I)
         `("Hours" ,year-row-seconds ,seconds⊥hours)))
    `("Year"
       ,(table-transformer 'plays
         `("#" ,year-row-rank-year ,I)
         `("Year" ,year-row-year-year ,I)
         `("Plays" ,year-row-plays-year ,n⊥s)))))

(← (render-main-years years)
  (for (title '(h2 "Years"))
       (← count (ι 0 years))
       (← top-years (ι 1 years))
       (← table (render-main-years-table (↑n 15 top-years)))
       (desc `(p ,(n⊥s count) " different years explored."))
       (more '(h3 (a (@ (href "./years/1.html")) "More")))
       (yield `(,title ,desc ,table ,more))))

(← (render-main artists albums songs genres countries years)
  (for (name "lol.fm")
       (head (render-main-head))
       (← artists-html (render-main-top-artists artists))
       (← albums-html (render-main-albums albums))
       (← songs-html (render-main-songs songs))
       (← genres-html (render-main-genres genres))
       (← countries-html (render-main-countries countries))
       (← years-html (render-main-years years))
       (contents ($ html `(,name ,@head ,artists-html ,albums-html 
                           ,songs-html ,genres-html ,countries-html
                           ,years-html)))
       (filename "lolfm.html")
       (← ok? (write-html "/tmp/lolfm" filename contents))
       (yield ok?)))
