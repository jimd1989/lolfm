(include-relative "../helpers/prelude.scm")
(include-relative "../html/common.scm")
(include-relative "../repos/years.scm")

(← render-years-table
  (tabbed-table-transformer "years"
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

(← (render-years paginated-rows)
  (for (← page (ι 0 paginated-rows))
       (← next (ι 1 paginated-rows))
       (← rows (ι 2 paginated-rows))
       (← table (render-years-table rows))
       (name "Years")
       (next-link (? next `(h3 (a (@ (href ,(◇ "./" next ".html"))) "More")) ∅))
       (contents (html name `(h1 ,name) table next-link))
       (filename (◇ page ".html"))
       (← ok? (write-html "/tmp/lolfm/years" filename contents))
       (yield ok?)))
