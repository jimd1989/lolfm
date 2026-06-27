(import (chicken file) (chicken load) (chicken string) srfi-1 sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../html/common.scm")
(include-relative "../transformers/common.scm")

(← render-country-artists-table
  (tabbed-table-transformer
    `("Plays" ,(table-transformer
                 `("#" rank ,I)
                 `("Artist" name ,I)
                 `("Plays" plays ,I)))
    `("Hours" ,(table-transformer
                 `("#" rank ,I)
                 `("Artist" name ,I)
                 `("Hours" hours ,I)))))

(← (render-country-artists ω)
  (for (← plays (∈ 'plays ω))
       (← hours (∈ 'hours ω))
       (← id (∈ 'id plays))
       (← name (∈ 'name plays))
       (← plays-artists (∈ 'artists plays))
       (← hours-artists (∈ 'artists hours))
       (← table (render-country-artists-table `(,plays-artists ,hours-artists)))
       (html `(html (head (title ,name)) (body (h1 ,name) ,table)))
       (filename (◇ id ".html"))
       (← ok? (write-html "/tmp/lolfm/countries" filename html))
       (yield ok?)))

(← render-top-countries-table
  (tabbed-table-transformer
    `("Plays" ,(table-transformer
                 `("#" countries-row-country-rank-plays ,n⊥s)
                 `("Country" countries-row-country-name-plays ,I)
                 `("Plays" countries-row-country-plays ,n⊥s)))
    `("Hours" ,(table-transformer
                 `("#" countries-row-country-rank-seconds ,n⊥s)
                 `("Country" countries-row-artist-name-seconds ,I)
                 `("Hours" countries-row-country-seconds ,seconds⊥hours)))))

(← (render-top-countries ω)
  (for (table (render-top-countries-table `(,plays ,hours)))
       (html `(html (head (title "Top countries"))
                    (body (h1 "Top countries") ,table)))
       (_ (print html))
       (← ok? (write-html "/tmp/lolfm/countries" "index.html" html))
       (yield ok?)))
