#lang racket

(define chunk-start '(#\( #\[ #\{ #\<))
(define chunk-end '(#\) #\] #\} #\>))

(define start-to-end '((#\( . #\))
                       (#\[ . #\])
                       (#\{ . #\})
                       (#\< . #\>)))

(define (chunk-start? ch)
  (assoc ch start-to-end))

(define (chunk-end? ch)
  (member ch chunk-end))

(define (read-chunk line)
  (cond
    [(string=? line "") '()]
    [(chunk-start? (string-ref line 0))
     (read-chunkk (string-ref line 0) line 1)]))

;; [<>({}){}[([])<>]]
(define (read-chunkk start line pos)
  (define len (string-length line))
  (cond
    [(<= len pos) 'incomplete]
    [(char=? (cdr (assoc start start-to-end))
             (string-ref line pos)) (add1 pos)]
    [(chunk-start? (string-ref line pos)) (let loop ([new-pos (read-chunkk (string-ref line pos)
                                                                           line
                                                                           (add1 pos))])
                                            (cond
                                              [(symbol? new-pos) new-pos]
                                              [(<= len pos) 'incomplete]
                                              [(char=? (string-ref line new-pos)
                                                       (cdr (assoc start start-to-end))) (add1 new-pos)]
                                              [(chunk-start? (string-ref line new-pos)) (loop (read-chunkk (string-ref line new-pos)
                                                                                                           line
                                                                                                           (add1 new-pos)))]))]
    [(chunk-end? (string-ref line pos)) 'corrupted]))

(module+ test
  (require rackunit)
  (read-chunk "()()")
  (read-chunk "[<>({}){}[(])<>]]")
  )

