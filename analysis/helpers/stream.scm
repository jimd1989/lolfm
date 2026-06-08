(import (chicken io) (chicken load) (chicken process))
(include-relative "syntax.scm")

(← STREAM-EOF 'STREAM-EOF)
(← (stream-end? ω) (eq? 'STREAM-EOF ω))

; maybe

(← (stream-cmd ω)
  (∃ ((port (open-input-pipe ω)) (eof? #f) (buffer ∅))
    (λ (#!optional (mode 'pop) (γ  ∅))
      (cond ((eq? mode 'push) (set! buffer (⊂ γ buffer)))
            (eof? STREAM-EOF)
            ((not (∅? buffer)) (∃ ((β (↑ buffer))) (set! buffer (↓ buffer)) β))
            (else (∃ ((α (read-line port)))
                    (? (eof-object? α)
                       (begin (close-input-pipe port) (set! eof? #t) STREAM-EOF)
                       α)))))))

(← (transducer f)
  (∃ ((buf ∅))
    (λ (ω #!optional (mode 'pop) (α ∅))
        (cond ((eq? mode 'push) (set! buf (⊂ α buf)))
              ((∧ (stream-end? ω) (∅? buf)) ω)
              ((stream-end? ω) (∃ ((β (↑ buf))) (set! buf (↓ buf)) β))
              (else
                (∃ ((Ω (f ω)))
                  (? (∅? buf) Ω
                    (∃ ((β (↑ buf))) (set! buf `(,@(↓ buf) ,Ω)) β))))))))

(← (stream-take n s) 
  (∃ ((ω (s))) (cond ((stream-end? ω) `(,STREAM-EOF))
                     ((= n 1) `(,ω)) 
                     (else (⊂ ω (stream-take (- n 1) s))))))

;OLD

(← (stream-while p s)
  (∃ ((ω (s)))
    (cond ((∅? ω) ∅)
          ((p ω) (⊂ ω (stream-while p s)))
          (else (begin (s 'push ω) ∅)))))

(← (stream-group f s #!optional (prev ∅))
  (∃ ((ω (s))) 
    (cond ((∅? ω) ∅)
          ((∅? prev) (⊂ ω (stream-group f s (f ω))))
          (else (∃ ((α (f ω)))
                     (? (equal? α prev)
                        (⊂ ω (stream-group f s α))
                        (begin (s 'push ω) ∅)))))))

(← (stream-map f s)
  (∃ ((ω (s))) (? (∅? ω) ω (⊂ (f ω) (stream-map f s)))))

(← (stream-close s) (stream-while (K #t) s))

(← (stream-sql db α) (stream-cmd (◇ "sqlite3 -tabs " db " " "\"" α "\"")))
