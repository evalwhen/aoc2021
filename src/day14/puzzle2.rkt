#lang racket

(define (parse-rule line)
  (let* ([parts (string-split line " -> ")]
         [pair (string->list (first parts))]
         [c (string-ref (second parts) 0)]
         [v (list (list (first pair) c)
                  (list c (second pair)))])
    (values pair v)))

(define (parse-data filename)
  (call-with-input-file filename
    (lambda (in)
      (define template (string->list (read-line in)))
      ;; skip empty line
      (read-line in)
      (define rules (for/hash ([line (in-lines in)])
                      (parse-rule line)))
      (define rule (lambda (pattern)
                     (hash-ref rules pattern)))
      (values template rule))))

(define-values (template rule) (parse-data "input2.txt"))

(module+ test
  template
  (rule (string->list "CH")))

;; 上层信息会随着分裂（递归）继承到下层
(define (step seqs n)
  (if (zero? n)
      (for/fold ([counter (hash)])
                ([pair (in-list seqs)])
        (hash-update counter (second pair) add1 0))
      (for/fold ([counter (hash)])
                ([pair (in-list seqs)])
        (for/fold ([counter counter])
                  ([(ch c) (in-hash (step (rule pair) (- n 1)))])
          (hash-update counter ch (lambda (v) (+ v c)) 0)))))

(define (bootstrap n)
  (define seq (for/list ([a (in-list template)]
                         [d (in-list (cdr template))])
                (list a d)))
  (define count (step seq n))
  (hash-update count (car template) add1 0))

(module+ test
  (bootstrap 10))

(define (puzzle n)
  (define res (bootstrap n))

  (- (apply max (hash-values res))
     (apply min (hash-values res))))

(module+ test
  ;; (step2 (string-ref template 0) (substring template 1) "")
  (time (puzzle 10))
  (time (puzzle 40))
  )
