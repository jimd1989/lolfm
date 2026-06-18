(import (chicken fixnum) (chicken load))
(include-relative "../helpers/syntax.scm")

(← (n⊥s n)
  (letrec ((ωs (string->list (number->string n)))
           (▽ (λ (αs θ) (cond ((∅? αs) ∅)
                              ((= θ 3) (⊂ #\, (▽ αs 0)))
                              (else (⊂ (↑ αs) (▽ (↓ αs) (+ θ 1))))))))
    ($ ◇ (⊖ (▽ (⊖ ωs) 0)))))

(← (seconds⊥hours n) (n⊥s (fx/ (fx/ n 60) 60)))
