(import (chicken io) (chicken load) (chicken process))
(include-relative "../helpers/syntax.scm")

(← (cmd→stream ω)
  (∃ ((port (open-input-pipe ω)))
    (λ () (∃ ((α (read-line port)))
      (? (eof-object? α) (begin (close-input-pipe port) α) α)))))

(← (stream⇒ f acc ωs)
  (∃ ((ω (ωs))) (? (eof-object? ω) acc (stream⇒ f (f acc ω) ωs))))

(← (stream-sql db α) (cmd→stream (◇ "sqlite3 -tabs " db " " "\"" α "\"")))
