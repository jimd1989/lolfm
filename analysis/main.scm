(import (chicken load) (chicken pretty-print))
(include-relative "./helpers/monad.scm")
(include-relative "./helpers/syntax.scm")
(include-relative "./repos/countries.scm")
(include-relative "./transformers/countries.scm")

(← DB "~/.config/cmus/lolfm.db")

(pp
  (for (← all-countries ((get-countries DB) transform-individual-countries))
       (← ok? (sequence all-countries))
       (yield all-countries))
)
