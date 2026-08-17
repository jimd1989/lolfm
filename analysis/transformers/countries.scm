(← transform-individual-countries
  (∘ (†⊙ (†⊆v? countries-row-country-id-plays))
     (†⊙ († (D ⍋⊆v! 'plays (O < countries-row-artist-rank-plays))))
     (†⊙ († (D ⍋⊆v! 'seconds (O < countries-row-artist-rank-seconds))))
     (†<* († render-country-artists))
     (†⊙ († (D ⊆vι 0)))))

(← sort-top-countries
  (∘ (D ⍋⊆v! 'plays (O < countries-row-country-rank-plays))
     (D ⍋⊆v! 'seconds (O < countries-row-country-rank-seconds))
     (D ⍋⊆v! 'year (O < countries-row-country-rank-year))))
