(import (chicken pretty-print))
(← DB "./lolfm.db")

(pp
  (for (← _ (write-css "/tmp/lolfm" "style.css" css))
       (← artist-pages ((get-artist-pages DB) transform-artist-pages))
       (← all-countries ((get-countries DB) transform-individual-countries))
       (top-countries (sort-top-countries all-countries))
       (← _ (render-top-countries top-countries))
       (← top-artists ((get-artists DB) transform-top-artists))
       (← top-albums ((get-albums DB) transform-albums))
       (← top-songs ((get-songs DB) transform-songs))
       (← top-genres ((get-genres DB) transform-genres))
       (← top-years ((get-years DB) transform-years))
       (← recent-plays ((get-plays DB) transform-plays))
       (← ok? (render-main top-artists top-albums top-songs top-genres
                           all-countries top-years))
       (yield top-albums))
)
