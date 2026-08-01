(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/songs.scm")

(← transform-songs
  (∘ (†⊙ (†⊆ 256))
     (†⊙ (†§))
     (†<* († render-songs))
     (†>>= († (D ι 2)))))
