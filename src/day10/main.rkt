#lang racket

(define start-to-end '((#\( . #\))
                       (#\[ . #\])
                       (#\{ . #\})
                       (#\< . #\>)))

(define scores '((#\) . 3)
                 (#\] . 57)
                 (#\} . 1197)
                 (#\> . 25137)))

(define complete-score '((#\) . 1)
                         (#\] . 2)
                         (#\} . 3)
                         (#\> . 4)))
(define (chunk-start? ch)
  (assoc ch start-to-end))

(define (read-chunk line)
  (cond
    [(string=? line "") '()]
    [(chunk-start? (string-ref line 0))
     (read-remaining2 (string-ref line 0) line 1 '())]))

(define (get-close start)
  (cdr (assoc start start-to-end)))

;; [<>({}){}[([])<>]]
;; {([(<{}[<>[]}>{[]{[(<()>
(define (read-remaining start line pos)
  (define len (string-length line))
  (cond
    [(<= len pos) (list 'incomplete)]
    [(char=? (cdr (assoc start start-to-end))
             (string-ref line pos))
     (add1 pos)]
    [(chunk-start? (string-ref line pos))
     (let loop ([new-pos (read-remaining (string-ref line pos)
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
          (loop (read-remaining (string-ref line new-pos)
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

;; [()]
(define (read-remaining2 start line pos missing)
  (define len (string-length line))
  (define close (get-close start))
  (cond
    ;; incomplete
    [(<= len pos) (read-remaining2 start
                                   (string-append line (string close))
                                   pos
                                   (cons close missing))]
    [(char=? close (string-ref line pos))
     (list 'success
           (add1 pos)
           missing
           line)]
    [(chunk-start? (string-ref line pos))
     (let loop ([new-pos (read-remaining2 (string-ref line pos)
                                          line
                                          (add1 pos)
                                          missing)])
       ;; TODO: refactor.
       (cond
         [(equal? (car new-pos) 'corrupted) new-pos]
         [(<= (string-length (last new-pos)) (second new-pos))
          (read-remaining2 start
                           (string-append (last new-pos) (string close))
                           (second new-pos)
                           (cons close (caddr new-pos)))]
         [(char=? (string-ref (last new-pos) (second new-pos))
                  close)
          (list 'success
                (add1 (second new-pos))
                (caddr new-pos)
                (last new-pos))]
         [(chunk-start? (string-ref (last new-pos) (second new-pos)))
          (loop (read-remaining2 (string-ref (last new-pos) (second new-pos))
                                 (last new-pos)
                                 (add1 (second new-pos))
                                 missing))]
         [else (list 'corrupted
                     close
                     (string-ref (last new-pos) (second new-pos))
                     (last new-pos)
                     (second new-pos))]))]
    [else
     (list 'corrupted
           close
           (string-ref (string-append line (list->string missing)) pos)
           (string-append line (list->string missing))
           pos)]))


(module+ test
  (read-chunk "[(()[<>])]({[<{<<[]>>( ")
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

(define (score closes)
  (for/fold ([sum 0])
            ([close closes])
    (+ (* sum 5) (cdr (assoc close complete-score)))))

(define (puzzle2 filename)

  (define scores '())
  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)])
        (let ([res (read-chunk line)])
          (cond
            [(and (list? res)
                  (equal? (car res) 'success)
                  (not (null? (caddr res))))
             (set! scores (append scores (list (score (reverse (caddr res))))))])))))
  (define middle-idx (floor (/ (length scores) 2)))
  (when (not (= middle-idx 0)) (list-ref (sort scores <) middle-idx))

  scores
  )
(module+ test
  (score (reverse '(#\> #\} #\) #\])))
  ;; (puzzle1 "input1.txt")
  ;; (puzzle1 "input2.txt")

  (puzzle2 "input1.txt")
  )
