(import (chicken bitwise) (chicken fixnum) (chicken load) (chicken sort) srfi-1
        srfi-4)
(include-relative "monad.scm")
(include-relative "prelude.scm")
(include-relative "syntax.scm")

(define-record-type slice
  (make-slice length sorts vec)
  slice?
  (length slice-length slice-length-set!)
  (sorts slice-sorts slice-sorts-set!)
  (vec slice-vec slice-vec-set!))

(← (next-2 n)
  (∃ ((n (- n 1))
      (n (bitwise-ior n (arithmetic-shift n -1)))
      (n (bitwise-ior n (arithmetic-shift n -2)))
      (n (bitwise-ior n (arithmetic-shift n -4)))
      (n (bitwise-ior n (arithmetic-shift n -8)))
      (n (bitwise-ior n (arithmetic-shift n -16)))
      (n (bitwise-ior n (arithmetic-shift n -32))))
    (+ n 1)))

(← (slice n) (make-slice 0 ∅ (make-vector (next-2 n))))

(← ⊆v slice) (← ⊆vρ slice-length) (← ⊆vv slice-vec) (← ⊆v⍋ slice-sorts)
(← ⊆vρ! slice-length-set!) (← ⊆vv! slice-vec-set!) (← ⊆v⍋! slice-sorts-set!)
(← vρ vector-length)
(← (vι n ω) (vector-ref ω n)) ; unsafe since slice checks length anyway
(← (v! n α ω) (vector-set! ω n α))

(← (copy-vector! α ω n)
  (∃▽ ((▽ (λ (m) (? (= m n) #t (begin (v! m (vι m α) ω) (▽ (+ m 1))))))) (▽ 0)))

(← (grow-slice! α)
  (∃ ((l (vρ (⊆vv α))))
    (? (> (⊆vρ α) (fx/ l 2))
      (∃ ((ω (make-vector (fx* 2 l))))
         (copy-vector! (⊆vv α) ω (⊆vρ α))
         (⊆vv! α ω) α)
      α)))

(← (slice-append! α ω)
  (∃ ((l (⊆vρ ω)))
     (v! l α (⊆vv ω)) (⊆vρ! ω (+ l 1)) (⊆v⍋! ω ∅) (grow-slice! ω)))

(← (slice->vector ω) (subvector (slice-vec ω) 0 (slice-length ω)))
(← ⊆v⊥v slice->vector) (← ⊆v⊥xs (∘ vector->list ⊆v⊥v))

(← (ordering f ω n) (list->u64vector (⍋ (O f (D (⍨ vι) ω)) (iota n))))
(← (slice-sort f ω) (ordering f (⊆vv ω) (⊆vρ ω)))
(← (slice-sort! α f ω)
  (∃ ((⍋s (⊆v⍋ ω))) (⊆v⍋! ω `((,α ,(slice-sort f ω)) ,@⍋s))) ω)
(← ⍋⊆v! slice-sort!)

(← (sort-fold f acc ω ⍋ω)
  (∃▽ ((▽ (λ (n α) (? (< n 0)
                     α
                     (∃ ((ι64 (u64vector-ref ⍋ω n)) (x (vι ι64 ω)))
                       (▽ (- n 1) (f x α)))))))
      (▽ (- (u64vector-length ⍋ω) 1) acc)))

(← (sorted-slice-fold f acc k ω)
  (for (← ⍋ω (∈ k (⊆v⍋ ω)))
       (yield (sort-fold f acc (⊆vv ω) ⍋ω))))
(← ⊆v⍋⇒ sorted-slice-fold)
