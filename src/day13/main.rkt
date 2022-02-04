#lang racket

(define (line-to-dots line)
  (match (string-split line ",")
    [(list v1 v2) (cons (string->number v1)
                        (string->number v2))]))

(define (parse-data filename)
  (define dots (make-hash))

  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)]
                 #:break (string=? line ""))
        (hash-set! dots (line-to-dots line) #t))))
  dots)

(define trans-fold
  (lambda (at)
    (lambda (v)
      (- (* 2 at) v))))

(define (fold-up dots at)
  (define res (make-hash))

  (define converter (trans-fold at))
  (for* ([x (in-range 0 4000)]
         [y (in-range 0 at)])
    (when (hash-ref dots (cons x (converter y)) #f)
      (hash-set! res (cons x y) #t))
    (when (hash-ref dots (cons x y) #f)
      (hash-set! res (cons x y) #t)))
  res)

;; todo, remove dup
(define (fold-left dots at)
  (define res (make-hash))

  (define converter (trans-fold at))

  (for* ([x (in-range 0 at)]
         [y (in-range 0 3000)])
    (when (hash-ref dots (cons (converter x) y) #f)
      (hash-set! res (cons x y) #t))
    (when (hash-ref dots (cons x y) #f)
      (hash-set! res (cons x y) #t)))
  res
  )

(module+ test
  (hash-count (parse-data "input2.txt"))
  ;; ((trans-fold 7) 0)
  (hash-count (fold-up (parse-data "input1.txt") 7))
  (hash-count (fold-left (parse-data "input2.txt") 655))
  )
