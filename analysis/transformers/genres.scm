(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/genres.scm")

(← transform-genres
  (∘ (†⊙ (†⊆ 256))
     (†⊙ (†§))
     (†<* († render-genres))
     (†>>= († (D ι 2)))))
