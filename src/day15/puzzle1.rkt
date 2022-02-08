#lang racket

(define (neibors pos m)
  (let* ([x (car pos)]
         [y (cdr pos)])
    (define raw `((,x . ,(add1 y))
                  (,(add1 x) . ,y)))
    (define (valid-p pos)
      (and (<= 0 (car pos) m)
           (<= 0 (cdr pos) m)))

    (filter valid-p raw)
    raw))

(module+ test
  (neibors (cons 0 0) 2))

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
  (parse-data "test.txt"))

(define risk-level (lambda (data pos) (hash-ref data pos)))

(define-syntax-rule (define/memo (name arg ...) body0 body ...)
  (define name
    (let ([memo (make-hash)])
      (lambda (arg ...)
        (define k (list arg ...))
        (cond
          [(hash-ref memo k #f)]
          [else (let ([res (begin body0 body ...)])
                  (begin0 res
                    (hash-set! memo k res)))])))))

(define/memo (find-min data start end)
  (if (equal? start end)
      (risk-level data end)
      (for/fold ([m 10000])
                ([neibor (neibors start 2)]
                 #:when (hash-has-key? data neibor))
        (min m (+ (risk-level data start)
                  (find-min data neibor end))))
      ))

(define (min-path data start end)
  (define m (- (sqrt (hash-count data)) 1))
  (define/memo (find-min start)
    (if (equal? start end)
      (risk-level data end)
      (for/fold ([m 1000000000000000])
                ([neibor (neibors start m)]
                 #:when (hash-has-key? data neibor))
        (min m (+ (risk-level data start)
                  (find-min neibor))))
      ))
  (find-min start))

(define puzzle
  (lambda (filename)
    (let* ([data (parse-data filename)]
           [dim (- (sqrt (hash-count data)) 1)])

      (define res (min-path data (cons 0 0) (cons dim dim)))

      (- res (hash-ref data (cons 0 0)))
      res)))

(module+ test

  (time (puzzle "input2.txt"))

  ;; (time (puzzle "input2.txt"))
  )
