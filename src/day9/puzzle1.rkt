#lang racket

(define (get-neibors curr max-row max-col)
  (define (valid pos limit) (and (< pos limit) (>= pos 0)))

  (define res '())

  (define directions '(up down left right))

  (define (generate-pos direction)
    (let* ([row (car curr)]
           [col (cdr curr)]
           [up (- row 1)]
           [down (+ row 1)]
           [left (- col 1)]
           [right (+ col 1)])
      (case direction
        [(up) (if (valid up max-row) (cons up col) #f)]
        [(down) (if (valid down max-row) (cons down col) #f)]
        [(left) (if (valid left max-col) (cons row left) #f)]
        [(right) (if (valid right max-col) (cons row right) #f)]
        [else (error 'invalid)])))

  (let loop ([directions directions] [res '()])
    (cond
      [(null? directions) res]
      [(generate-pos (car directions)) => (lambda (pos)  (loop (cdr directions) (cons pos res)))]
      [else (loop (cdr directions) res)])))


(define matrix #(#(1 2 3 4)
                 #(5 6 7 8)
                 #(10 11 12 13)
                 #(14 15 16 17)))

(define parse-input
  (lambda (input)
    (let* ([lines (string-split input "\n")]
           [matrix (make-vector (length lines))])
      (for ([line lines]
            [i (in-naturals 0)])
        (let ([row (make-vector (string-length line))])
          (for ([c (in-string line)]
                [j (in-naturals 0)])
            (vector-set! row j (string->number (string c))))
          (vector-set! matrix i row)))
      matrix)))

(define get-input
  (lambda (filename)
    (call-with-input-file filename
      (lambda (in) (port->string in)))))


(define (matrix-ref mat i j)
  (vector-ref (vector-ref mat i) j))

(define (puzzle1 matri)
  (let ([rows-length (vector-length matri)]
        [col-length (vector-length (vector-ref matri 0))]
        [res 0]
        [lst '()])
    (for* ([i (in-range 0 rows-length)]
           [j (in-range 0 col-length)])
      (let* ([neibors-pos (get-neibors (cons i j) rows-length col-length)]
             [neibors (map (lambda (pos) (matrix-ref matri (car pos) (cdr pos))) neibors-pos)]
             [min-neibor (apply min neibors)]
             [current-val (matrix-ref matri i j)])
        (when (< current-val min-neibor)
          ;;(set! res (append res (list current-val)))
          (set! res (+ res current-val 1))
          (set! lst (append lst (list current-val)))
          )))
    res))

;;(puzzle1 (parse-input (get-input "input1.txt")))
(puzzle1 (parse-input (get-input "input2.txt")))
