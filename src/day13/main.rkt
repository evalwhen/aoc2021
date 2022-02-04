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
    [(list col row) (cons (string->number row)
                          (string->number col))]
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

      (values dots ins))))

(define (do-fold2 dots ins)
  (define (convert ins pos)
    (define row (car pos))
    (define col (cdr pos))

    (match ins
      [(struct instruction ('up at))
       (cons (if (< row at) row (- at (- row at))) col)]
      [(struct instruction ('left at))
       (cons row (if (< col at) col (- at (- col at))))]))

  (for/hash ([(pos v) (in-hash dots)])
    (values (convert ins pos) v)))

(define (puzzle1 filename)
  (let-values ([(dots ins) (parse-data filename)])
    (do-fold2 dots (car ins))))

(define (puzzle2 filename)
  (let-values ([(dots ins) (parse-data filename)])
    (let loop ([dots dots] [ins ins])
      (cond
        [(null? ins) (display-dots dots)]
        [else (loop (do-fold2 dots (car ins)) (cdr ins))]))))

(define (display-dots dots)
  (for ([row (in-inclusive-range 0 (apply max (map car (hash-keys dots))))])
    (for ([col (in-inclusive-range 0 (apply max (map cdr (hash-keys dots))))])
      (if (hash-has-key? dots (cons row col))
          (display #\#)
          (display #\Space)))
    (newline)))

(module+ test
  (parse-data "input1.txt")
  (hash-count (puzzle1 "input1.txt"))
  (hash-count (puzzle1 "input2.txt"))

  ;;GJZGLVPJ
  (puzzle2 "input2.txt")
  )
