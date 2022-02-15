#lang racket

(define (parse-literal input)
  (define  (stop-p group)
    (char=? (string-ref group 0) #\0))

  (let loop ([in input] [out ""])
    (let* ([group (substring in 0 5)]
           [part (substring group 1)])
      ;; (println group)
      (cond
        [(stop-p group) (string-append out part)]
        [else (loop (substring in 5)
                    (string-append out part))]))))

(define (bits-to-decimal bits)
  (define last-pos (- (string-length bits) 1))

  (let loop ([pos 0] [n last-pos] [res 0])
    (cond
      [(= pos last-pos) (+ res (string->number (string (string-ref bits pos))))]
      [else (loop (add1 pos)
                  (sub1 n)
                  (+ res (* (expt 2 n)
                            (string->number (string (string-ref bits pos))))))])))
(module+ test
  ;; (bits-to-decimal "1111")
  (bits-to-decimal "0000101")
  (bits-to-decimal (parse-literal "101111111000101000"))
  )
