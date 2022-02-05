#lang racket

(define (parse-rule line)
  (let ([parts (string-split line " -> ")])
    (values (car parts) (cadr parts))))

(define (parse-data filename)
  (call-with-input-file filename
    (lambda (in)
      (define template (read-line in))
      ;; skip empty line
      (read-line in)
      (define rules (for/hash ([line (in-lines in)])
                      (parse-rule line)))
      (define rule (lambda (pattern)
                     (hash-ref rules pattern #f)))
      (values template rule))))

(define-values (template rule) (parse-data "input1.txt"))

(module+ test
  template
  (rule "CH"))

(define (step str curr next res)
  (define len (string-length str))
  (cond
    [(= next len) (string-append res (string (string-ref str curr)))]
    [(rule (substring str curr (+ next 1)))
     => (lambda (c) (step str
                          (add1 curr)
                          (add1 next)
                          (string-append res
                                         (string (string-ref str curr))
                                         c)))]
    [else (step str
                (add1 curr)
                (add1 next)
                (string-append res
                               (string (string-ref str curr))))]))

(module+ test
  (step (step template 0 1 "") 0 1 ""))
