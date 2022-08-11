#!/usr/local/bin/csi -s

(import (chicken io) (chicken process) (chicken string) srfi-1)

(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define-syntax ∃ (syntax-rules () ((_ . α) (let* . α))))

(define-constant FIFO "/tmp/lolfm-fifo")

(define-constant FIELDS
  '(status duration title album artist albumartist genre date))

(define-constant NUMERICAL '(duration date))

(define-constant FALLBACK
 '((status unknown)
   (duration 0)
   (title "Unknown Title")
   (album "Unknown Album")
   (artist "Unknown Artist")
   (genre "None")
   (date 0)))

(define (apostrophe-quote x)
  (list->string
    (foldr 
      (λ (x acc) (if (char=? #\' x) (cons #\' (cons #\' acc)) (cons x acc)))
      '()
      (string->list x))))


(define (parse-field x)
  (∃ ((split (string-split x))
      (words (if (string=? "tag" (car split)) (cdr split) split))
      (key (string->symbol (car words)))
      (val (cond ((eq? key 'status) (string->symbol (cadr words)))
                 ((member key NUMERICAL) (string->number (cadr words)))
                 ((member key FIELDS) 
                  (string-intersperse (map apostrophe-quote (cdr words)) " "))
                 (else 'invalid))))
     (if (equal? val 'invalid) #f `(,key ,val))))

(define (check-albumartist xs)
  (if (assoc 'albumartist xs)
      xs 
      (append `((albumartist ,(cadr (assoc 'artist xs)))) xs)))

(define (parse-fields xs)
  (∃ ((f (λ (α ω) (∃ ((ok (parse-field α))) (if ok (append `(,ok) ω) ω))))
      (parsed (foldr f '() xs))
      (compare (λ (α ω) (equal? (car α) (car ω)))))
    (check-albumartist (lset-union compare parsed FALLBACK))))

(define (no-nonsense xs)
  (foldl (λ (acc x) (or acc (string=? "tag" (car (string-split x))))) #f xs))

(define (main)
  (∃ ((fn (open-output-file FIFO))
      (report (with-input-from-pipe "cmus-remote -Q" read-lines))
      (fields (parse-fields report))
      (event `(log ,fields)))
     (if (no-nonsense report)
         (begin
           (write event fn)
           (newline fn)
           (close-output-port fn)))))

(main)
