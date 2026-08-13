(import (chicken io) (chicken process))

(← (cmd→stream ω)
  (∃ ((port (open-input-pipe ω)))
    (λ () (∃ ((α (read-line port)))
      (? (eof-object? α) (begin (close-input-pipe port) α) α)))))

; if transducer is using call/cc, consider "mapping" in the resouce lifecycle:
; things need to be a bit inside-out here. think about it
;(← (cmd→stream cmd handler)
;  (∃ ((port (open-input-pipe cmd)))
;    (dynamic-wind
;      (λ () #f)
;      (λ () (handler (λ () (read-line port))))
;      (λ () (close-input-pipe port)))))

(← (stream⇒ f acc ωs)
  (∃ ((ω (ωs))) (? (eof-object? ω) acc (stream⇒ f (f acc ω) ωs))))

(← (stream-sql db α) (cmd→stream (◇ "sqlite3 -tabs " db " " "\"" α "\"")))

(← (s⊥ f e ω) (>>= (λ (α) (ensure α (◇ e ": " ω) α)) (either (f ω))))
(← (s⊥n ω) (s⊥ string->number "not number" ω))
(← (s⊥x ω) (s⊥ string->symbol "not valid symbol" ω))
(← (s⊥s ω) (right ω))
(← s⊥b (∘ (◁ (D = 1)) s⊥n))
(← (decoder key f) (λ (ω) (⊙ (λ (α) `(,key ,α)) (f ω))))
(← (decode decoders row) (sequence (∀ $$ decoders (string-split row "\t"))))
(← (decode-record r parsers row) (⊙ (D $ r) (decode parsers row)))

(define-syntax define-sql-record
  (er-macro-transformer
    (lambda (form rename compare?)
      (let* ((name (↑↓ form))
             (body (↓↓ form))
             (make-name (string->symbol (◇ "make-" name)))
             (decode-name (string->symbol (◇ "decode-" name)))
             (name? (string->symbol (◇ name "?")))
             (fields (∀ ↑ body))
             (accessors (∀ (lambda (ω) (string->symbol (◇ name "-" ω))) fields))
             (decoders (∀ ↑↓ body)))
        `(,(rename 'begin)
           (,(rename 'define-record-type)
             ,(rename name)
             (,(rename make-name) ,@(∀ rename fields))
             ,(rename name?)
             ,@(∀ (lambda (α ω) (list (rename α) (rename ω))) fields accessors))
           (,(rename 'define)
             (,(rename decode-name) ω)
             (,(rename 'decode-record)
               ,(rename make-name)
               (list ,@(∀ rename decoders)) ω))))))) 
