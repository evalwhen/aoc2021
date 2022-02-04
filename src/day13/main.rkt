#lang racket

(struct instruction (type at))

(define (line-to-instruction line)
  (let ([parts (string-split (third (string-split line " "))
                             "=")])
    (if (string=? "x" (first parts))
        (instruction 'left (string->number (second parts)))
        (instruction 'up (string->number (second parts))))))

(define (line-to-dots line)
  (match (string-split line ",")
    [(list v1 v2) (cons (string->number v1)
                          (string->number v2))]
    [_ #f]))

(define (parse-data filename)
  (define dots (make-hash))
  (define max-x 0)
  (define max-y 0)
  (define ins '())

  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)]
                 #:unless (string=? line ""))
        (match (line-to-dots line)
          [(cons x y)
           (when (> x max-x) (set! max-x x))
           (when (> y max-y) (set! max-y y))
           (hash-set! dots (cons x y) #t)]
          [_
           (set! ins (append ins (list (line-to-instruction line))))]))))
  (values dots max-x max-y ins))

(define trans-fold
  (lambda (at)
    (lambda (v)
      (- (* 2 at) v))))

(define (fold-up dots at)
  (do-fold dots
           3000
           at
           (lambda (x) x)
           (trans-fold at)))

(define (fold-left dots at)
  (do-fold dots
           at
           3000
           (trans-fold at)
           (lambda (x) x)))


(define (do-fold dots xlen ylen xc yc)
  (define res (make-hash))

  (for* ([x (in-range 0 xlen)]
         [y (in-range 0 ylen)])

    (when (hash-ref dots (cons x y) #f)
      (hash-set! res (cons x y) #t))

    (when (hash-ref dots (cons (xc x) (yc y)) #f)
      (hash-set! res (cons x y) #t)))
  res)

(define (puzzle1 filename)
  (let-values ([(dots mx my ins) (parse-data filename)])
    (match (first ins)
      [(struct instruction ('up at))
       (fold-up dots at)]
      [(struct instruction ('left at))
       (fold-left dots at)])))

(module+ test
  ;; (parse-data "input1.txt")
  ;; ((trans-fold 7) 0)
  (hash-count (puzzle1 "input1.txt"))
  (hash-count (puzzle1 "input2.txt"))
  ;; (hash-count (fold-up (parse-data "input1.txt") 7))
  ;; (hash-count (fold-left (parse-data "input2.txt") 655))
  )
