(← render-artist-page-table
  (tabbed-table-transformer "artist-page"
    `("Albums"
       ,(table-transformer 'album-plays
         `("#" ,artist-page-row-album-rank ,I)
         `("Album" ,artist-page-row-album-title ,I)
         `("Plays" ,artist-page-row-album-plays ,n⊥s)))
    `("Songs"
       ,(table-transformer 'song-plays
         `("#" ,artist-page-row-song-rank ,I)
         `("Song" ,artist-page-row-song-title ,I)
         `(,(loved #t) ,artist-page-row-loved? ,loved)
         `("Plays" ,artist-page-row-song-plays ,n⊥s)))))

(← (render-artist-page-flag code id)
   `(span (@ (class "artist-flag")) 
          ,(link "../countries/" id (render-flag code))))

(← (render-artist-pages rows)
  (for (head (⊆vι 0 rows))
       (name (◇ " " (artist-page-row-artist-name head)))
       (flag (render-artist-page-flag (artist-page-row-country-code head)
                                      (artist-page-row-country-id head)))
       (id (artist-page-row-artist-id head))
       (← table (render-artist-page-table rows))
       (contents (html name `(h1 ,flag ,name) table))
       (filename (◇ id ".html"))
       (← ok? (write-html "/tmp/lolfm/artist-page" filename contents))
       (yield ok?)))
