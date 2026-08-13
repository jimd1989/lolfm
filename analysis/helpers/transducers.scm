(import (chicken io) (chicken process) srfi-1)

; Unlike iterators, transducers pre-compose chains of functions that are only
; reified against data later, not unlike Haskell fusion. A given transducer in 
; a composition chain is defined
(← (t-pure f) (λ (r) (λλ (() (r))
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

(← inc (t-pure (λ (n) (+ n 1))))

; monadic control in terms of Either, where f is itself an actual transducer
; applies reducer to f and runs it conditionally, short-circuiting with a
; unilateral flush from dyadic branch on Left
(← (t-m f wrap)
  (λ (r)
    (∃ ((g ((∘ f (t-pure wrap)) r)))
      (λλ (() (r))
          ((acc) (? (left? acc) (r acc) (g acc)))
          ((acc ω) (? (left? ω) (r ω) (g acc (get ω))))))))

(← (t-map f) (t-m f right))
(← (t-bind f) (t-m f I))

(← † t-pure) (← †⊙ t-map) (← †>>= t-bind)

; 1:n stream transducer → emit ω n times linearly
(← (t-clone n)
  (λ (r)
    (λλ (() (r))
        ((acc) 
         (letrec ((▽ (λ (m α) (? (= m n) (r α) (▽ (+ 1 m) (r α))))))
           (▽ 1 acc)))
        ((acc ω)
          (letrec ((▽ (λ (m) (? (= m n) (r acc ω) (r (▽ (+ 1 m)) ω)))))
            (▽ 1))))))

; statefully delegate (↑ fs) to ω
(← (t-gear . fs)
  (λ (r)
    (∃ ((gs ($ circular-list
               (∀ (λ (f) (f (λλ (() (r)) ((acc) acc) ((acc ω) (r acc ω))))) 
                  fs))))
      (λλ (() (r))
          ((acc)
           (letrec ((▽ (λ (n α hs)
                         (? (= 0 n) (r α)  (▽ (- n 1) ((↑ hs) α) (↓ hs))))))
             (▽ (ρ fs) acc gs)))
          ((acc ω) 
           (∃ ((α ((↑ gs) acc ω))) (set! gs (↓ gs)) α))))))

; concurrently run fs sub-transducers → emit results linearly (can always chunk)
(← (t-mux . fs) (∘ (t-clone (ρ fs)) ($ t-gear fs)))

(← (t-unit) (λ (r) (λλ (() (r)) ((acc) acc) ((acc ω) acc))))

(← †*** t-gear) (← †&&& t-mux) (← †∅ t-unit) 

; for void functions fs
(← (tap f) (∘ (t-mux f (t-pure I)) (t-gear (t-unit) (t-pure I))))

(← (tap-m f) (∘ (t-mux f (t-pure right)) (†⊙ (t-gear (t-unit) (t-pure I)))))

(← †<< tap) (← †<$ tap-m) (← (†<* f) (†>>= (†<$ f))) 

; slightly more complex: stateful transducers can call reduce conditionally.
; f = item/buffer transformer
; p = buffer yield predicate
; g = buffer yield transformer
; h = buffer post-yield transformer
; i = optional flush buffer yield transformer
(← (t-until f p g h #!optional (i g))
  (λ (r)
    (∃ ((buf ∅))
       (λλ (() (r))
           ((acc)
            (? (∅? buf) (r acc) (∃ ((α (i buf))) (set! buf ∅) (r (r acc α)))))
           ((acc ω) 
            (set! buf (f ω buf))
            (? (p buf) (∃ ((α (g buf))) (set! buf (h buf)) (r acc α)) acc))))))

(← (t-chunk n) (t-until ⊂ (∘ ((C =) n) ρ) ⊖ (K ∅)))

(← (t-chunk-on f)
  (t-until ⊂ 
           (λ (buf) (∧ (> (ρ buf) 1) ((J (∘ ¬ ≡) (∘ f ↑) (∘ f ↑↓)) buf)))
           (∘ ⊖ ↓)
           (λ (buf) `(,(↑ buf)))
           ⊖))

(← (t-until-slice f p g h #!optional (i g))
  (λ (r)
    (∃ ((buf (slice)))
       (λλ (() (r))
           ((acc)
            (? (⊆v∅? buf)
              (r acc)
              (∃ ((α (i buf))) (set! buf (slice)) (r (r acc α)))))
           ((acc ω)
            (set! buf (f ω buf))
            (? (p buf) (∃ ((α (g buf))) (set! buf (h buf)) (r acc α)) acc))))))

(← (t-chunk-slice n) (t-until-slice ⊆v⊂ (∘ (D = n) ⊆vρ) ⊆v⊥⊆v (D ⊆vρ! 0) I))

(← (t-chunk-on-slice f)
  (t-until-slice
    ⊆v⊂
    (λ (buf) (∧ (> (⊆vρ buf) 1)
                ((J (∘ ¬ ≡) (∘ f (D ⊆vι -1)) (∘ f (D ⊆vι -2))) buf)))
    (J ⊆vρ! (∘ (D + -1) ⊆vρ) ⊆v⊥⊆v)
    (λ (buf) (v! 0 (⊆vι -1 buf) (⊆vv buf)) (⊆vρ! 1 buf))
    I))
; another tricky one. holds ω in memory until n matches arrive, then releases
; all grouped together. Unlike other transducers, does not flush anything
; partial
(← (t-join-on n f)
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

(← (t-filter p)
  (λ (r) (λλ (() (r)) ((acc) acc) ((acc ω) (? (p ω) (r acc ω) acc)))))

; wrap every item in (page next-page? item)
(← (t-paginate)
  (λ (r)
    (∃ ((buf ∅) (n 0))
      (λλ (() (r))
          ((acc) (? (∅? buf) (r acc) (r (r acc buf))))
          ((acc ω)
           (set! n (+ n 1))
           (∃ ((β `(,n #f ,ω)))
             (? (∅? buf)
               (begin (set! buf β) acc)
               (∃ ((m (↑ buf)) (α `(,m ,(+ m 1) ,(↑ (↓↓ buf)))))
                 (set! buf β) (r acc α)))))))))


(← †⊆ t-chunk) (← †⊆? t-chunk-on) (← †⊆v t-chunk-slice)
(← †⊆v? t-chunk-on-slice) (← †↕ t-join-on) (← †? t-filter)
(← †§ t-paginate)

; traversal → how ωs is traversed: foldl, etc
; pipeline  → the pipeline, transducer itself
; reduce    → combines all results into reified final value
; acc       → empty accumulator state, just like a fold
; ωs        → raw inputs, could be physical, could be port
(← (transduce traversal pipeline reduce acc ωs)
   (call/cc (λ (△)
              (∃ ((step (λλ ((α ω) (reduce α ω)) ((α) (△ α))))
                  (f (pipeline step)))
                (f (traversal f acc ωs))))))

(← †⇒ transduce)
