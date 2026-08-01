(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/albums.scm")

(← transform-albums
  (∘ (†⊙ (†⊆ 256))
     (†⊙ (†§))
     (†<* († render-albums))
     (†>>= († (D ι 2)))))
