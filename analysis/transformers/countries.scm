(import (chicken load))
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/countries.scm")

(← transform-individual-countries
  (∘ (†⊙ (†⊆v? countries-row-country-id-plays))
     (†⊙ († (D ⍋⊆v! 'plays (O < countries-row-artist-rank-plays))))
     (†⊙ († (D ⍋⊆v! 'seconds (O < countries-row-artist-rank-seconds))))
     (†<* († render-country-artists))
     (†⊙ († (D ⊆vι 0)))))
