(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define ∘ compose) 

(import (chicken io))

(define (drain)
  (let* ((fn (open-input-file "/tmp/fifo"))
         (xs (read-list fn)))
    (display xs) (newline)
    (close-input-port fn)))

(define (main)
  (drain)
  (main))

(main)
