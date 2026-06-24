(import (chicken load) (chicken string) srfi-1 sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../html/common.scm")
(include-relative "../transformers/common.scm")

(← render-top-countries-table
  (tabbed-table-transformer
    `("Plays" ,(table-transformer
                 `("#" rank ,n⊥s)
                 `("Country" name ,I)
                 `("Plays" plays ,I)))
    `("Hours" ,(table-transformer
                 `("#" rank ,n⊥s)
                 `("Country" name ,I)
                 `("Hours" hours ,I)))))

(← (render-top-countries ω)
  (for (← plays (∈ 'plays ω))
       (← hours (∈ 'hours ω))
       (← table (render-top-countries-table `(,plays ,hours)))
       (yield table)))
