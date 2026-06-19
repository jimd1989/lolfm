(import (chicken io) (chicken load) (chicken process) srfi-1)
(include-relative "prelude.scm")
(include-relative "syntax.scm")

; Unlike iterators, transducers pre-compose chains of functions that are only
; reified against data later, not unlike Haskell fusion. A given transducer in 
; a composition chain is defined
(← (⊙t f) (λ (r) (λλ (() (r))
                     ((acc) (r acc))
                     ((acc ω) (r acc (f ω))))))
; where
;  f   = primary data transformer (a → b)
;  r   = function to combine results (c → b → c) not strictly monoidal
;  acc = previous value in composition chain (think folds)
;  ω   = current value in composition chain (think folds)
; the case-lambda (acc) is for "flushing remaining data", which isn't a problem
; in pure idiomatic transducers, but needs to be plumbed through towards any
; stateful batched transducers.
; the niladic case-lambda () is apparently needed to conform to the cannonical
; definition of transducers, but it seems like a convenient constructor more
; than anything else

(← inc (⊙t (λ (n) (+ n 1))))

; slightly more complex: stateful transducers can call reduce conditionally.
; f = item/buffer transformer
; p = buffer yield predicate
; g = buffer yield transformer
; h = buffer post-yield transformer
; i = optional flush buffer yield transformer
(← (until f p g h #!optional (i g))
  (λ (r)
    (∃ ((buf ∅))
       (λλ (() (r))
           ((acc)
            (? (∅? buf) (r acc) (∃ ((α (i buf))) (set! buf ∅) (r (r acc α)))))
           ((acc ω) 
            (set! buf (f ω buf))
            (? (p buf) (∃ ((α (g buf))) (set! buf (h buf)) (r acc α)) acc))))))

(← (chunk n) (until ⊂ (∘ ((C =) n) ρ) ⊖ (K ∅)))

(← (chunk-on f)
  (until ⊂ 
         (λ (buf) (∧ (> (ρ buf) 1) ((J (∘ ¬ ≡) (∘ f ↑) (∘ f ↑↓)) buf)))
         (∘ ⊖ ↓)
         (λ (buf) `(,(↑ buf)))
         ⊖))

; more difficult: calling n child pipelines, then outputting them into a flat
; stream. most complexity comes from proper flushing of all children.
; needs to pass the unary case to all children to flush subordinate pipes, then
; flatten them with the parent reducer, them emit that.
; in both unary and dyadic cases, the result must be immediately reified to a
; literal list before emitted into the rest of the pipeline.
; honestly not 100% sure this shit works

; possibly a more general "identity" helper: reify to list on dyadic, thing
; itself for unary.
(← It (λλ ((acc) acc) ((acc ω) (⊃ acc ω))))

(← (mux . fs)
  (λ (r)
    (∃ ((gs (∀ ((C &) It) fs)))
      (λλ (() (r))
          ((acc) (∃ ((flushed (⇐ (λ (α g) (g α)) ∅ gs)))
                   (∃ ((αs (⇐ (λ (α ω) (r α ω)) acc flushed))) (r αs))))
          ((acc ω) (∃ ((αs (⇐ (λ (α g) (g α ω)) ∅ gs))) (⇐ r acc αs)))))))

; for a void function f (not easy to inject monad here)
; not ideal but might need "poisoned" transducers
(← (tap . fs)
   (∃ ((l (ρ fs)))
     (∘ ($ mux (⊃ fs (⊙t I))) (chunk (+ 1 l)) (⊙t (D ↓n l)) (⊙t ↑))))

; where f is $> or *>
(← (tap-m f . fs)
   (∃ ((l (ρ fs)))
     (∘ ($ mux (⊃ fs (⊙t I)))
        (chunk (+ 1 l)) 
        (⊙t (λ (ω) (∃ ((voids (↑n l ω)) (α (↑ (↓n l ω))))
                     (f (sequence voids) α)))))))

; another tricky one. holds ω in memory until n matches arrive, then releases
; all grouped together. Unlike other transducers, does not flush anything
; partial
(← (join-on n f)
  (λ (r)
    (∃ ((matches ∅))
      (λλ (() (r))
          ((acc) (r acc))
          ((acc ω)
           (∃ ((key (f ω))
               (match? (assoc key matches))
               (all-matches (? match? `(,ω ,@(↓ match?)) `(,ω))))
             (set! matches (alist-update key all-matches matches))
             (? (= n (ρ all-matches))
               (begin (set! matches (alist-update key ∅ matches))
                      (r acc all-matches))
               (begin (set! matches (alist-update key all-matches matches))
                      acc))))))))

(← (filter-t p)
  (λ (r) (λλ (() (r)) ((acc) acc) ((acc ω) (? (p ω) (r acc ω) acc)))))

; traversal → how ωs is traversed: foldl, etc
; pipeline  → the pipeline, transducer itself
; reduce    → combines all results into reified final value
; acc       → empty accumulator state, just like a fold
; ωs        → raw inputs, could be physical, could be port

(← (transduce traversal pipeline reduce acc ωs #!optional (flush? #t))
  (∃ ((step (λλ ((α ω) (reduce α ω)) ((α) α)))
      (f (pipeline step)))
     (∃ ((res (traversal f acc ωs)))
       (? flush? (f res) res))))

;(transduce ⇐ (∘ inc (mux (⊙t (K 100)) inc) inc (chunk 4))  ⊃ ∅ (list 1 2 3) #t)
;(transduce ⇐ (∘ (join-on ↑ 2))  ⊃ ∅ '((a 1) (b 2) (a 3) (c 1)) #t)
