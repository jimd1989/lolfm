(import (chicken load) (chicken string))
(include-relative "monad.scm")
(include-relative "syntax.scm")

(← (s⊥ f e ω) (>>= (λ (α) (ensure α (◇ e ": " ω) α)) (either (f ω))))
(← (s⊥n ω) (s⊥ string->number "not number" ω))
(← (s⊥s ω) (right ω))
(← (decoder key f) (λ (ω) (⊙ (λ (α) `(,key ,α)) (f ω))))
(← (decode decoders row) (sequence (∀ $$ decoders (string-split row "\t"))))
