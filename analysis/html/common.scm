(import (chicken load) (chicken string) srfi-1 sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/syntax.scm")
(include-relative "../helpers/transducers.scm")
(include-relative "../transformers/common.scm")

; figure out css, file naming, etc
(← (write-html file ω)
  (for (← written? (either (with-output-to-file file (λ () (SXML->HTML ω)))))
       (← success? (ensure written? (◇ "Error writing html file: " file) #t))
       (yield success?)))

(← (tag α . ω) (⊂ α ω))

(← column-title ↑)
(← column-key ↑↓)
(← column-transformer (∘ ↑ ↓↓))

(← (row-transformer . columns)
  (∃ ((fs (∀ (λ (c) (∘ (◁ (D tag 'td))
                       (◁ (D $$ (column-transformer c)))
                       (D ∈ (column-key c)))) columns)))
    (λ (row) (⊙ (D $ tag 'tr) (traverse (D & row) fs)))))

(← (table-transformer . columns)
  (∃ ((titles (⊂ 'tr (∀ (∘ (D tag 'th) column-title) columns)))
      (row-f ($ row-transformer columns))
      (wrapper (∘ (D tag 'table) (D tag 'tbody))))
    (λ (table) (⊙ (∘ wrapper (D ⊂ titles)) (traverse row-f table)))))

(← tabbed-table-name ↑)
(← tabbed-table-f ↑↓)

(← (tabbed-table-transformer . named-table-transformers)
  (λ (tables)
    (for (inputs
          (∀ (λ (tab n) (∃ ((ω (◇ "recent-" n))
                            (name (tabbed-table-name tab)))
                          `((input (@ (id ,ω) (type radio)))
                            (label (@ (for ,ω)) name))))
               named-table-transformers
               (iota (ρ named-table-transformers) 1)))
         (← rendered 
           (∀ (λ (tab t)
                `(section (@ (class tab-panel)) ,((tabbed-table-f tab) t)))
              named-table-transformers
              tables))
         (yield `(div (@ (class tabset))
                   ,@inputs
                   (div (@ (class tab-panels)) ,@rendered))))))

; scratch
(← EXAMPLE 
   '(div (@ (id "content") checked)
         (h1 "Welcome")
         (p "This is a " (strong "nested") " layout.")))
