#lang racket

(define (big-cave-p cave)
  (char-upper-case? (string-ref cave 0)))

(define (small-cave-p cave)
  (char-lower-case? (string-ref cave 0)))

(define (parse-data filename)
  (define map (make-hash))

  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)])
        (let* ([parts (string-split line "-")]
               [k (first parts)]
               [v (second parts)])
          (hash-set! map
                     k
                     (cons v (hash-ref map k null)))

          (hash-set! map
                     v
                     (cons k (hash-ref map v null)))
          ))))
  map)


(module+ test
  (parse-data "input3.txt")
  )

(define graph (parse-data "input3.txt"))

(define (connects start)
  (hash-ref graph start null))

(define (append-start start paths)
  (let loop ([paths paths] [res '()])
    (cond
      [(null? paths) res]
      [else (loop (cdr paths) (cons (cons start (car paths))
                                    res))])))
(define (has-twice visited)
  (for/or ([(k v) (in-hash visited)])
    (and (small-cave-p k)
         (= v 2))))

(define (paths start end)

  (define (allow-visit visited vertex)
    (if (or (string=? vertex start)
            (string=? vertex end))
        (not (hash-ref visited vertex #f))
        (or (big-cave-p vertex)
            (not (hash-ref visited vertex #f))
            (not (has-twice visited)))))

  (letrec ([A (lambda (start visited)
                (if (string=? start end)
                    `((,end))
                    (for/fold ([res '()])
                              ([conn (connects start)]
                               #:when (allow-visit visited conn))
                      (append res
                              (append-start start (A conn (hash-set visited
                                                                    start
                                                                    (+ 1 (hash-ref visited start 0)))))))))])
    (A start (hash))))

(module+ test
  ;; (paths "start" "end")
  (length (paths "start" "end"))
  )
