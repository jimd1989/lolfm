(import (chicken io) (chicken process) (chicken string) srfi-1)

(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define-syntax ∃ (syntax-rules () ((_ . α) (let* . α))))
(define-syntax ← (syntax-rules ()
  ((_ ((α ω) ...)) (begin (define α ω) ...))
  ((_ . α) (define . α))))
(define-syntax Λ (syntax-rules (% ▽ <>)
  ((_ ▽ () (ω ...)) (cut ω ...))
  ((_ ▽ (% β ...) (ω ...)) (Λ ▽ (β ...) (ω ... <>)))
  ((_ ▽ (α β ...) (ω ...)) (Λ ▽ (β ...) (ω ... α)))
  ((_ α ...) (Λ ▽ (α ...) ()))))

(← ((◇ conc) (⊖ flip) (∘ compose) (∀ map) (↑ car) (⊣ cons)
    (↓ cdr) (I identity) ($ apply) (∈ (⊖ member)) (∅ '())))
(← ((s→x string->symbol) (s→n string->number)
    (s→xs string-split) (xs→s string-intersperse)))
(← (K α) (λ (ω) α))
(← (? p f g) (λ (α) (if (p α) (f α) (g α))))
(← (?? p f g) (λ (α) (∃ ((ω (p α))) (if ω (f ω) (g α)))))
(← (⊥ f . α) (λ (ω) ($ f `(,@α ,ω))))
(← (◀ . fs) (Λ foldr (λ (f ω) (call/cc (λ (g) ((? I f g) ω)))) % fs))
(← (&&& . fs) (λ (α) (∀ (Λ % α) fs)))
(← (*** . fs) (⊥ ∀ (Λ % %) fs))
(← (split-list p)
  (⊥ foldr (λ (α ω) ((? p (K (⊣ ∅ ω)) (K `(,(⊣ α (↑ ω)) ,@(↓ ω)))) α)) '(())))
(← (∴ f g) (&&& (∘ f ↑) (∘ g ↓)))
(← (∴∴ f g) (∘ flatten (∴ f g)))
