(import (chicken fixnum) (chicken load) (chicken string) srfi-1 sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/sorted-slices.scm")
(include-relative "../helpers/syntax.scm")

; figure out css, file naming, etc
(← (write-html dir file ω)
  (for (← directory-created? (either (create-directory dir #t)))
       (path (◇ dir "/" file))
       (_ (print (◇ "writing " path)))
       (← written? (either (with-output-to-file path (λ () (SXML->HTML ω)))))
       (← success? (ensure written? (◇ "Error writing html file: " file) #t))
       (yield success?)))

(← (tag α . ω) (⊂ α ω))

(← column-title ↑)
(← column-key ↑↓)
(← column-transformer (∘ ↑ ↓↓))

(← (row-transformer . columns)
  (∃ ((fs (∀ (λ (c) (∘ (D tag 'td) (column-transformer c) (column-key c)))
             columns)))
    (λ (row) (⊂ 'tr (∀ (D & row) fs)))))

(← (table-transformer sort-key . columns)
  (∃ ((titles (⊂ 'tr (∀ (∘ (D tag 'th) column-title) columns)))
      (row-f ($ row-transformer columns))
      (wrapper (∘ (D tag 'table) (D tag 'tbody))))
    (λ (table) ((∘ (◁ wrapper) (◁ (D ⊂ titles))) (⊆v⍋∀ row-f sort-key table)))))

(← tabbed-table-name ↑)
(← tabbed-table-f ↑↓)

(← (render-table-inputs named-transformer n)
  (∃ ((ω (◇ "recent-" n)) (name (tabbed-table-name named-transformer)))
    `((input (@ (id ,ω) (type radio)))
      (label (@ (for ,ω)) ,name))))

(← (render-table rows named-transformer)
  (⊙ (λ (ω) `(section (@ (class tab-panel)) ,ω))
     ((tabbed-table-f named-transformer) rows)))

(← (render-tabbed-table inputs contents)
  `(div (@ (class tabset)) ,@inputs (div (@ (class tab-panels)) ,@contents)))

(← (tabbed-table-transformer . fs)
  (λ (rows)
    (for (inputs (∀ render-table-inputs fs (iota (ρ fs) 1)))
         (← contents (traverse (D render-table rows) fs))
         (yield (render-tabbed-table inputs contents)))))

(← (n⊥s n)
  (letrec ((ωs (string->list (number->string n)))
           (▽ (λ (αs θ) (cond ((∅? αs) ∅)
                              ((= θ 3) (⊂ #\, (▽ αs 0)))
                              (else (⊂ (↑ αs) (▽ (↓ αs) (+ θ 1))))))))
    ($ ◇ (⊖ (▽ (⊖ ωs) 0)))))

(← (seconds⊥hours n) (n⊥s (fx/ (fx/ n 60) 60)))
