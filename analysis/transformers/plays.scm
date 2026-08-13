(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/plays.scm")

(← transform-plays
  (∘ (†⊙ (†⊆ 2048))
     (†⊙ (†§))
     (†<* († render-plays))
     (†>>= († (D ι 2)))))
