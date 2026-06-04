(import (chicken io) (chicken load) (chicken process))
(include-relative "syntax.scm")

(← (stream-cmd ω)
  (∃ ((port (open-input-pipe ω)) (eof? #f))
    (λ () (? eof? ∅ (∃ ((α (read-line port)))
                      (? (eof-object? α)
                        (begin (close-input-pipe port) (set! eof? #t) ∅)
                        α))))))

(← (stream-take n s) 
  (∃ ((ω (s))) (cond ((∅? ω) ∅) 
                     ((= n 1) `(,ω)) 
                     (else (⊂ ω (stream-take (- n 1) s))))))

(← (stream-map f s)
  (∃ ((ω (s))) (? (∅? ω) ω (⊂ (f ω) (stream-map f s)))))

(← (stream-until p s)
  (∃ ((ω (s))) (cond ((∅? ω) ∅) ((p ω) `(,ω)) (else (⊂ ω (stream-until p s))))))

(← (stream-close s) (stream-until (K #f) s))

(← (stream-sql db α) (stream-cmd (◇ "sqlite3 -tabs " db " " "\"" α "\"")))
