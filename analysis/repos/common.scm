(import (chicken io) (chicken load) (chicken process))
(include-relative "../helpers/syntax.scm")

(← (cmd→stream ω)
  (∃ ((port (open-input-pipe ω)))
    (λ () (∃ ((α (read-line port)))
      (? (eof-object? α) (begin (close-input-pipe port) α) α)))))

; if transducer is using call/cc, consider something like this
;(← (with-cmd→stream ω body-proc)
;  (∃ ((port (open-input-pipe ω)))
;    (dynamic-wind
;      (λ () #f)
;      (λ () (body-proc (λ () 
;                         (∃ ((α (read-line port))) 
;                           α)))) ; Clean generator, no inline close needed
;      (λ () (close-input-pipe port))))) ; GUARANTEED to close on early exit

(← (stream⇒ f acc ωs)
  (∃ ((ω (ωs))) (? (eof-object? ω) acc (stream⇒ f (f acc ω) ωs))))

(← (stream-sql db α) (cmd→stream (◇ "sqlite3 -tabs " db " " "\"" α "\"")))
