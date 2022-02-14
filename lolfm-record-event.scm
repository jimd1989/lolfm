#!/usr/local/bin/csi -s

(import (chicken io) (chicken process) (chicken process-context) srfi-1)

(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define ∘ compose) 

(define (pairs xs)
  (cond ((null? xs) '())
        ((= (length xs) 1) '())
        (else (append `((,(car xs) ,(cadr xs))) (pairs (cddr xs))))))

(define (parse-fields xs)
  (let* ((fields '(status artist album title))
         (f (λ (x) (if (string=? "status" (car x))
                            `(status ,(string->symbol (cadr x)))
                            `(,(string->symbol (car x)) ,(cadr x)))))
         (fs (map f (pairs xs))))
    (filter (λ (x) (member (car x) fields)) fs)))

;(define date-cmd "date '+%d %b %Y, %H:%M'")
(define date-cmd "date '+%s'")
(define (date) (call-with-input-pipe date-cmd (∘ string->number read-line)))

(define main
  (let* ((fn (open-output-file "/tmp/fifo"))
         (args (pairs (command-line-arguments)))
         (date `((date ,(date))))
         (fields (append date (parse-fields (command-line-arguments))))
         (event `(log ,fields)))
    (write event fn)
    (newline fn)
    (close-output-port fn)))

(main)
