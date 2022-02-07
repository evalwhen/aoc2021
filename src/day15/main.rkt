#lang racket

(define (neibors pos)
  (let ([x (car pos)]
        [y (cdr pos)])
    `((,x . ,(add1 y))
      (,(add1 x) . ,y))))

(module+ test
  (neibors (cons 0 0)))

(define (parse-data filename)
  (call-with-input-file filename
    (lambda (in)
      (for/fold ([data (hash)])
                ([line (in-lines in)]
                 [x (in-naturals 0)])
        (for/fold ([data data])
                  ([c (in-string line)]
                   [y (in-naturals 0)])
          (hash-set data (cons x y) (string->number (string c))))))))

(module+ test
  (hash-count (parse-data "input1.txt")))

(define (add-front a lsts)
  (letrec ([A (lambda (a lsts res)
                (cond
                  [(null? lsts) res]
                  [else (A a (cdr lsts) (cons (cons a (car lsts)) res))]))])
    (A a lsts '())))

(define risk-level (lambda (data pos) (hash-ref data pos)))

(define (paths data start end)

  (letrec ([A (lambda (start end res)
                (if (equal? start end)
                    (+ res (risk-level data end))
                    (for/fold ([v 100000])
                              ([neibor (neibors start)]
                               #:when (hash-has-key? data neibor))
                      (min v (A neibor end (+ res (risk-level data start)))))))])
    (A start end 0)))

(define puzzle1
  (lambda (filename)
    (let* ([data (parse-data filename)]
           [dim (- (sqrt (hash-count data))
                   1)])
      (apply min (map (lambda (path) (apply + (cdr path)))
                      (paths data
                             (cons 0 0)
                             (cons dim dim))))
      )))

(module+ test
  ;; (puzzle1 "input1.txt")
  ;; (puzzle1 "input2.txt")
  (paths (parse-data "input2.txt") (cons 0 0) (cons 99 99))
  )
