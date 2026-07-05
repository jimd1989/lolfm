(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/countries.scm")

(← transform-top-artists
  (∘ (†⊙ (†⊆ 512))
     (†⊙ (†§))))
