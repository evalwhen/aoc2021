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
  (call-with-input-file filename
    (lambda (in)
      (define dots (for/fold ([dots (hash)])
                             ([line (in-lines in)]
                              #:break (string=? line ""))
                     (hash-set dots (line-to-dots line) #t)))
      (define ins (for/list ([line (in-lines in)])
                    (line-to-instruction line)))

      (values dots ins)))
  )

(define (do-fold2 dots ins)
  (define (convert ins pos)
    (define x (car pos))
    (define y (cdr pos))

    (match ins
      [(struct instruction ('up at))
       (cons x (if (< y at) y (- at (- y at))))]
      [(struct instruction ('left at))
       (cons (if (< x at) x (- at (- x at))) y)]))

  (for/hash ([(pos v) (in-hash dots)])
    (values (convert ins pos) v)))

(define (puzzle1 filename)
  (let-values ([(dots  ins) (parse-data filename)])
    (do-fold2 dots (car ins))))

(define (puzzle2 filename)
  (let-values ([(dots ins) (parse-data filename)])
    (let loop ([dots dots] [ins ins])
      (cond
        [(null? ins) (display-dots dots)]
        [else (loop (do-fold2 dots (car ins)) (cdr ins))]))))

(define (display-dots dots)
  (for ([y (in-inclusive-range 0 (apply max (map cdr (hash-keys dots))))])
    (for ([x (in-inclusive-range 0 (apply max (map car (hash-keys dots))))])
      (if (hash-has-key? dots (cons x y))
          (display #\#)
          (display #\.)))
    (newline)))

(module+ test
  (parse-data "input1.txt")
  (hash-count (puzzle1 "input1.txt"))
  (hash-count (puzzle1 "input2.txt"))

  ;;GJZGLVPJ
  (puzzle2 "input2.txt")
  )
