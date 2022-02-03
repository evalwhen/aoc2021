#lang racket

(define (line-to-dots line)
  (match (string-split line ",")
    [(list v1 v2) (cons (string->number v1)
                        (string->number v2))]))

(define (parse-data filename)
  (call-with-input-file filename
    (lambda (in)
      (for/fold ([dots (hash)])
                ([line (in-lines in)]
                 #:break (string=? line ""))
        (hash-set dots (line-to-dots line) #t)))))

(define trans-fold
  (lambda (at)
    (lambda (v)
      (- (* 2 at) v))))

(module+ test
  ((trans-fold 7) 0))
