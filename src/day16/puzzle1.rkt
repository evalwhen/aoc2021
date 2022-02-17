#lang racket

(struct result (ver body remaining))

(define (parse-packet input res)
  null)

(define (parse-op input res)
  null)

(define (parse-literal ver input)
  (define  (stop-p group)
    (char=? (string-ref group 0) #\0))

  (let loop ([in input] [out ""])
    (let* ([group (substring in 0 5)]
           [part (substring group 1)])
      ;; (println group)
      (cond
        [(stop-p group) (result ver (string-append out part) (substring in 5))]
        [else (loop (substring in 5)
                    (string-append out part))]))))

(define (parse-op-by-len input len res)
  (case (packet-type input)
    [('literal) (let ([r (parse-literal input)])
                  (cond
                    [(= len (+ 6 (body-len r))) (cons r res)]
                    [else (parse-packet (result-remaining r) (cons r res))]))]
    [('op) (let ([r (parse-op input res)]
                 [header (op-header input)])
             (cond
               [(= len (+ (string-length header)
                          (body-len r)))
                (cons r res)]
               [else (parse-packet (result-remaining r) (cosn r res))]))]))

(define (body-len r)
  0)

(define (packet-type input)
  'literal)

(define (op-type input)
  'len)

(define (op-header input)
  "")
(define (bits-to-decimal bits)
  (define last-pos (- (string-length bits) 1))

  (let loop ([pos 0] [n last-pos] [res 0])
    (let ([posn (string->number (string (string-ref bits pos)))])
      (cond
        [(= pos last-pos) (+ res posn)]
        [else (loop (add1 pos)
                    (sub1 n)
                    (+ res (* (expt 2 n) posn)))]))))
(module+ test
  ;; (bits-to-decimal "1111")
  (bits-to-decimal "0000101")
  (bits-to-decimal (parse-literal "101111111000101000"))
  )
