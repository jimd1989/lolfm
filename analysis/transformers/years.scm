(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/years.scm")

(← transform-years
  (∘ (†⊙ (†⊆ 256))
     (†⊙ (†§))
     (†<* († render-years))
     (†>>= († (D ι 2)))))
