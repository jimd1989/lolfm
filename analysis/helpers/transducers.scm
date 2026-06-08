(import (chicken io) (chicken load) (chicken process))
(include-relative "prelude.scm")
(include-relative "syntax.scm")

; Unlike iterators, transducers pre-compose chains of functions that are only
; reified against data later, not unlike Haskell fusion. A given transducer in 
; a composition chain is defined
(← (transducer f) (λ (reduce) (λλ ((acc ω) (reduce acc (f ω)))
                                  ((acc)   (reduce acc)))))

; where
;  f      = primary data transformer (a → b)
;  reduce = function to combine results (c → b → c) not strictly monoidal
;  acc    = previous value in composition chain (think folds)
;  ω      = current value in composition chain (think folds)
; the case-lambda (acc) is for "flushing remaining data", which isn't a problem
; in pure idiomatic transducers, but needs to be plumbed through towards any
; stateful batched transducers.

(← increment (transducer (λ (n) (+ n 1))))
; increment            → (λ (reduce) (λ (acc ω) (reduce acc (+ 1 ω))))
; (increment +)        → (λ (acc ω) (+ acc (+ 1 ω)))
; ((increment +) 0 10) → (+ 0 (+ 10 1))

(← ones (transducer (K 1)))

; slightly more complex: stateful transducers can call reduce conditionally.
; f → item/buffer transformer
; p → buffer yield predicate
; g → buffer yield transformer
; h → buffer post-yield transformer
; i → optional flush buffer yield transformer
(← (until f p g h #!optional (i g))
  (λ (reduce)
    (∃ ((buf ∅))
       (λλ ((acc ω) 
            (set! buf (f ω buf))
            (? (p buf) (∃ ((α (g buf))) (set! buf (h buf)) (reduce acc α)) acc))
           ((acc)
            (? (∅? buf)
              (reduce acc)
              (∃ ((α (i buf))) (set! buf ∅) (reduce (reduce acc α)))))))))

(← (chunked n) (until ⊂ (∘ ((C =) n) ρ) ⊖ (K ∅)))

(← (chunk-on f)
  (until ⊂ 
         (λ (buf) (∧ (> (ρ buf) 1) ((J (∘ ¬ ≡) (∘ f ↑) (∘ f ↑↓)) buf)))
         (∘ ⊖ ↓)
         (λ (buf) `(,(↑ buf)))
         ⊖))

; transducers are composed as normal functions, since their outer lambda is
; unary.
(← pipeline (∘ increment increment increment (chunked 2)))
; pipeline            → (λ (reduce) ((∘ (λ (reduce) …)  …) reduce))
; (pipeline +)        → (λ (acc ω)  ((∘ (λ (acc ω) …) …) acc ω))
; ((pipeline +) 0 10) → 13

; pipeline → the pipeline, transducer itself
; reduce   → combines all results into reified final value
; acc      → empty accumulator state, just like a fold
; ωs       → raw inputs, could be physical, could be port
(← (transduce pipeline reduce acc ωs)
  (∃ ((step (λλ ((α ω) (reduce α ω)) ((α) α)))
      (f (pipeline step)))
    (f (⇐ f acc ωs))))

; (transduce pipeline + 0 '(10 20 30))
; (transduce pipeline (lambda (acc x) `(,@acc ,x)) '() (list 1 2 3 4))
