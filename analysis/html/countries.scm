(import (chicken file) (chicken load) (chicken string) srfi-1 sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/countries.scm")

(← render-country-artists-table
  (tabbed-table-transformer "country"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,countries-row-artist-rank-plays ,I)
         `("Artist" ,countries-row-artist-name-plays ,I)
         `("Plays" ,countries-row-artist-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,countries-row-artist-rank-seconds ,I)
         `("Artist" ,countries-row-artist-name-seconds ,I)
         `("Hours" ,countries-row-artist-seconds ,seconds⊥hours)))))

(← (render-country-artists rows)
  (for (← table (render-country-artists-table rows))
       (← head (either (⊆vι 0 rows)))
       (name (countries-row-country-name-plays head))
       (id (countries-row-country-id-plays head))
       (contents (html name `(h1 ,name) table))
       (filename (◇ id ".html"))
       (← ok? (write-html "/tmp/lolfm/countries" filename contents))
       (yield ok?)))

; OLD AND WRONG
;(← render-top-countries-table
;  (tabbed-table-transformer
;    `("Plays" ,(table-transformer
;                 `("#" countries-row-country-rank-plays ,n⊥s)
;                 `("Country" countries-row-country-name-plays ,I)
;                 `("Plays" countries-row-country-plays ,n⊥s)))
;    `("Hours" ,(table-transformer
;                 `("#" countries-row-country-rank-seconds ,n⊥s)
;                 `("Country" countries-row-artist-name-seconds ,I)
;                 `("Hours" countries-row-country-seconds ,seconds⊥hours)))))
;
;(← (render-top-countries ω)
;  (for (table (render-top-countries-table `(,plays ,hours)))
;       (html `(html (head (title "Top countries"))
;                    (body (h1 "Top countries") ,table)))
;       (_ (print html))
;       (← ok? (write-html "/tmp/lolfm/countries" "index.html" html))
;       (yield ok?)))
