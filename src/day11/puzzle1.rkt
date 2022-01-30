#lang racket

(define (neibors pos)
  `((,(car pos) . ,(sub1 (cdr pos)))
    (,(sub1 (car pos)) . ,(sub1 (cdr pos)))
    (,(sub1 (car pos)) . ,(cdr pos))
    (,(sub1 (car pos)). ,(add1 (cdr pos)))
    (,(car pos) . ,(add1 (cdr pos)))
    (,(add1 (car pos)) . ,(add1 (cdr pos)))
    (,(add1 (car pos)) . ,(cdr pos))
    (,(add1 (car pos)) . ,(sub1 (cdr pos)))))

(module+ test
  (neibors (cons 1 1)))

(define (flash-at input pos)
  (define flashed (make-hash))

  (letrec ([A (lambda (pos)
                (for ([neibor (neibors pos)]
                      #:unless (hash-ref flashed neibor #f)
                      #:when (hash-ref input neibor #f))
                  (B neibor (hash-ref input neibor))))]
           [B (lambda (pos v)
                (cond
                  [(< v 9)
                   (hash-set! input pos (+ v 1))
                   (values flashed input)]
                  [else
                   (hash-set! input pos 0)
                   (hash-set! flashed pos #t)
                   (A pos)
                   (values flashed input)]))])
    (cond
      [(hash-ref input pos #f) => (lambda (v) (B pos v))]
      [else (values flashed input)])))

(define (reset data flashed)
  (for ([pos (in-hash-keys flashed)])
    (hash-set! data pos 0)))

(define (puzzle1 filename)

  (define data (make-hash))

  (define flash-counter 0)

  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)]
            [i (in-naturals 0)])
        (for ([ch (in-string line)]
              [j (in-naturals 0)])
          (hash-set! data (cons i j) (string->number (string ch)))))))
  (for ([step (in-range 0 100)])
    (define step-flashed (make-hash))
    (for* ([i (in-range 0 10)]
           [j (in-range 0 10)])
      (let-values ([(flashed _) (flash-at data (cons i j))])
        (for ([pos (in-hash-keys flashed)])
          (hash-set! step-flashed pos #t))))

    (reset data step-flashed)
    (set! flash-counter (+ flash-counter (hash-count step-flashed))))

  flash-counter
  ;; data
  )

(module+ test
  ;; (flash-at (data small-input) (cons 1 1))
  (puzzle1 "input1.txt")
  )
