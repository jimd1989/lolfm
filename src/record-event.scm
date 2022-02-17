#!/usr/local/bin/csi -s

(import (chicken io) (chicken process-context) srfi-1)

; need to log albumartist
; genre?
; need placeholders for bad data

(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))

(define-constant FIFO "/tmp/fifo")

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

(define main
  (let* ((fn (open-output-file FIFO))
         (args (pairs (command-line-arguments)))
         (fields (parse-fields (command-line-arguments)))
         (event `(log ,fields)))
    (write event fn)
    (newline fn)
    (close-output-port fn)))

(main)
