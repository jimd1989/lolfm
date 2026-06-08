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

(← (chunk n) (until ⊂ (∘ ((C =) n) ρ) ⊖ (K ∅)))

(← (chunk-on f)
  (until ⊂ 
         (λ (buf) (∧ (> (ρ buf) 1) ((J (∘ ¬ ≡) (∘ f ↑) (∘ f ↑↓)) buf)))
         (∘ ⊖ ↓)
         (λ (buf) `(,(↑ buf)))
         ⊖))

; traversal → how ωs is traversed: foldl, etc
; pipeline  → the pipeline, transducer itself
; reduce    → combines all results into reified final value
; acc       → empty accumulator state, just like a fold
; ωs        → raw inputs, could be physical, could be port
(← (transduce traversal pipeline reduce acc ωs)
  (∃ ((step (λλ ((α ω) (reduce α ω)) ((α) α)))
      (f (pipeline step)))
    (f (traversal f acc ωs))))
