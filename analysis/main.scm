(import (chicken load) (chicken pretty-print))
(include-relative "./helpers/monad.scm")
(include-relative "./helpers/syntax.scm")
(include-relative "./repos/artists.scm")
(include-relative "./repos/countries.scm")
(include-relative "./transformers/artists.scm")
(include-relative "./transformers/countries.scm")

(← DB "~/.config/cmus/lolfm.db")

(pp
  (for (← _ (write-css "/tmp/lolfm" "style.css" css))
       (← all-countries ((get-countries DB) transform-individual-countries))
       (← all-artists ((get-artists DB) transform-top-artists))
       (yield (∀ countries-row-country-rank-year all-countries)))
)
