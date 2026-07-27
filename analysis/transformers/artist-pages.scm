(import (chicken load))
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/sorted-slices.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../html/artist-pages.scm")
(include-relative "../repos/artist-pages.scm")

(← transform-artist-pages
  (∘ (†⊙ (†⊆v? artist-page-row-artist-id))
     (†⊙ († (D ⍋⊆v! 'album-plays (O < artist-page-row-album-rank))))
     (†⊙ († (D ⍋⊆v! 'song-plays (O < artist-page-row-song-rank))))
     (†<* († render-artist-pages))))
