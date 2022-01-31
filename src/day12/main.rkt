#lang racket

(define (big-cave-p cave)
  (char-upper-case? (string-ref cave 0)))

(define (parse-data filename)
  (define map (make-hash))

  (call-with-input-file filename
    (lambda (in)
      (for ([line (in-lines in)])
        (let* ([parts (string-split line "-")]
               [k (first parts)]
               [v (second parts)]
               [has-big-cave (or (big-cave-p k)
                                 (big-cave-p v))])
          (hash-set! map
                     k
                     (cons v (hash-ref map k null)))

          (hash-set! map
                     v
                     (cons k (hash-ref map v null)))
          ))))
  map)


(module+ test
  ;; (parse-data "input1.txt")
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
(define (not-visited visited vertex)
  (or (big-cave-p vertex)
      (not (member vertex visited))))

(define (paths start end visited)
  (if (string=? start end)
      `((,end))
      (for/fold ([res '()])
                ([conn (connects start)]
                 #:when (not-visited visited conn))
        (append res (append-start start (paths conn end (cons start visited)))))))

(module+ test
  (length (paths "start" "end" '()))
  (paths "start" "end" '())
  )
