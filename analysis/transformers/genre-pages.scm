(← transform-genre-pages
  (∘ (†⊙ (†⊆v? genre-pages-row-genre-id))
     (†⊙ († (D ⍋⊆v! 'rank (O < genre-pages-row-rank))))
     (†<* († render-genre-pages))))
