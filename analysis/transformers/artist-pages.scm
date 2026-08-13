(← transform-artist-pages
  (∘ (†⊙ (†⊆v? artist-page-row-artist-id))
     (†⊙ († (D ⍋⊆v! 'album-plays (O < artist-page-row-album-rank))))
     (†⊙ († (D ⍋⊆v! 'song-plays (O < artist-page-row-song-rank))))
     (†<* († render-artist-pages))))
