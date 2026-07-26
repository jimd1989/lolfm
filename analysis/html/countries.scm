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
       (contents (html name `(h2 ,name) table))
       (filename (◇ id ".html"))
       (← ok? (write-html "/tmp/lolfm/countries" filename contents))
       (yield ok?)))

(← (top-country-link id name) (λ (ω) (link "./" (id ω) (name ω))))
(← top-country-link-plays 
  (top-country-link countries-row-country-id-plays 
                    countries-row-country-name-plays))
(← top-country-link-seconds
  (top-country-link countries-row-country-id-seconds 
                    countries-row-country-name-seconds))

(← render-top-countries-table
  (tabbed-table-transformer "top-countries"
    `("Plays"
       ,(table-transformer 'plays
         `("#" ,countries-row-country-rank-plays ,I)
         `("Country" ,I ,top-country-link-plays)
         `("Plays" ,countries-row-country-plays ,n⊥s)))
    `("Hours"
       ,(table-transformer 'seconds
         `("#" ,countries-row-country-rank-seconds ,I)
         `("Country" ,I ,top-country-link-seconds)
         `("Hours" ,countries-row-country-seconds ,seconds⊥hours)))
    `("Year"
      ,(table-transformer 'year
         `("#" ,countries-row-country-rank-year ,I)
         `("Country" ,I ,top-country-link-plays)
         `("Plays" ,countries-row-country-plays-year ,n⊥s)))))

(← (render-top-countries countries)
  (for (name "Countries")
       (filename "countries.html") 
       (← table (render-top-countries-table countries))
       (contents (html name `(h2 ,name) table))
       (← ok? (write-html "/tmp/lolfm/countries" filename contents))
       (yield ok?)))
