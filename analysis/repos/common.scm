(import (chicken io) (chicken load) (chicken process))
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/syntax.scm")

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
