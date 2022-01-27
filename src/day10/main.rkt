#lang racket

(define start-to-end '((#\( . #\))
                       (#\[ . #\])
                       (#\{ . #\})
                       (#\< . #\>)))

(define scores '((#\) . 3)
                 (#\] . 57)
                 (#\} . 1197)
                 (#\> . 25137)))

(define (chunk-start? ch)
  (assoc ch start-to-end))

(define (read-chunk line)
  (cond
    [(string=? line "") '()]
    [(chunk-start? (string-ref line 0))
     (read-chunkk (string-ref line 0) line 1)]))

;; [<>({}){}[([])<>]]
;; {([(<{}[<>[]}>{[]{[(<()>
(define (read-chunkk start line pos)
  (define len (string-length line))
  (cond
    [(<= len pos) (list 'incomplete)]
    [(char=? (cdr (assoc start start-to-end))
             (string-ref line pos))
     (add1 pos)]
    [(chunk-start? (string-ref line pos))
     (let loop ([new-pos (read-chunkk (string-ref line pos)
                                      line
                                      (add1 pos))])
       ;; TODO: refactor.
       (cond
         [(list? new-pos) new-pos]
         [(<= len new-pos) (list 'incomplete)]
         [(char=? (string-ref line new-pos)
                  (cdr (assoc start start-to-end)))
          (add1 new-pos)]
         [(chunk-start? (string-ref line new-pos))
          (loop (read-chunkk (string-ref line new-pos)
                             line
                             (add1 new-pos)))]
         [else (list 'corrupted
                     (cdr (assoc start start-to-end))
                     (string-ref line new-pos)
                     new-pos) ]))]
    [else
     (list 'corrupted
           (cdr (assoc start start-to-end))
           (string-ref line pos)
           pos)]))

(module+ test
  (require rackunit)
  (read-chunk "()()")
  (read-chunk "[<>({}){}[(])<>]]")
  (read-chunk "{([(<{}[<>[]}>{[]{[(<()>")
  )

(define (puzzle1 filename)

  (define corruptes '())
  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)])
        (let ([res (read-chunk line)])
          (cond
            [(and (list? res) (equal? (car res) 'corrupted))
             (set! corruptes (append corruptes (list (caddr res))))])))))
  (for/sum ([c (in-list corruptes)])
    (cdr (assoc c scores)))
  )

(module+ test
  (puzzle1 "input1.txt")
  (puzzle1 "input2.txt")
  )
