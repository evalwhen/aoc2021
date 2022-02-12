#lang racket

(define (neibors pos m)
  (let* ([x (car pos)]
         [y (cdr pos)])
    (define raw `((,x . ,(add1 y))
                  (,(add1 x) . ,y)))
    (define (valid-p pos)
      (and (<= 1 (car pos) m)
           (<= 1 (cdr pos) m)))

    (filter valid-p raw)))

;; (module+ test
;;   (neibors (cons 0 0) 2))

(define (parse-data filename)
  (call-with-input-file filename
    (lambda (in)
      (for/fold ([data (hash)])
                ([line (in-lines in)]
                 [x (in-naturals 1)])
        (for/fold ([data data])
                  ([c (in-string line)]
                   [y (in-naturals 1)])
          (hash-set data (cons x y) (string->number (string c))))))))

;; (module+ test
;;   (parse-data "input1.txt"))

(define risk-level (lambda (data pos) (hash-ref data pos)))

(define-syntax-rule (define/memo (name arg ...) body0 body ...)
  (define name
    (let ([memo (make-hash)])
      (lambda (arg ...)
        (define k (first (list arg ...)))
        (cond
          [(hash-ref memo k #f) => (lambda (lst) (values (first lst) (second lst)))]
          [else (let-values ([(v path) (begin body0 body ...)])
                  (begin0 (values v path)
                    (hash-set! memo k (list v path))
                    ;; (println k)
                    ;; (println res)
                    ))])))))

(define (find-min data start end)
  (if (equal? start end)
      (risk-level data end)
      (for/fold ([m 10000])
                ([neibor (neibors start 2)]
                 #:when (hash-has-key? data neibor))
        (min m (+ (risk-level data start)
                  (find-min data neibor end))))
      ))

(define (min-path data start end)
  (define m (sqrt (hash-count data)))

  ;; (define/memo (find-min start)
  ;;   (if (equal? start end)
  ;;     (risk-level data end)
  ;;     (for/fold ([m 1000000000000000])
  ;;               ([neibor (neibors start m)]
  ;;                #:when (hash-has-key? data neibor))
  ;;       (min m (+ (risk-level data start)
  ;;                 (find-min neibor))))
  ;;     ))

  (define (find-min start path)
    (if (equal? start end)
        (values (risk-level data end) path)
        (let-values ([(v path) (let ([nbs (neibors start m)])
                                 (if (= (length nbs) 2)
                                     (let-values ([(v1 path1) (find-min (first nbs) (cons start path))]
                                                  [(v2 path2) (find-min (second nbs) (cons start path))])

                                       (if (<= v1 v2)
                                           (values v1 path1)
                                           (values v2 path2)))
                                     (find-min (first nbs) (cons start path))))])
          (values (+ v (risk-level data start))
                  path))))
  (find-min start '()))

(define puzzle
  (lambda (filename)
    (let* ([data (parse-data filename)]
           [dim (sqrt (hash-count data))])

      ;; (define-values (res path) (min-path data (cons 0 0) (cons dim dim)))

      (min-path data (cons 1 1) (cons dim dim)))))
      ;; (- res (hash-ref data (cons 0 0))))))

(module+ test

  (time (puzzle "input1.txt"))

  ;; (time (puzzle "input2.txt"))
  )
