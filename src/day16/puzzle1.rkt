#lang racket

(struct result (type ver body remaining) #:transparent)

(define (parse-packet input)
  (case (packet-type input)
    [(literal) (parse-literal (packet-version input)
                              (packet-body input))]
    [(op) (parse-op (packet-version input) (packet-body input))]))

(define (parse-op ver input)
  (case (op-type input)
    [(len) (parse-op-by-len ver (op-body input 'len) (op-len input))]))

(define (parse-literal ver input)
  (define  (stop-p group)
    (char=? (string-ref group 0) #\0))

  ;; (println input)
  (let loop ([in input] [out '()])
    (let* ([group (substring in 0 5)]
           [part (substring group 1)])
      ;; (println group)
      (cond
        [(stop-p group) (result 'literal ver (reverse (cons part out)) (substring in 5))]
        [else (loop (substring in 5)
                    (cons part out))]))))

(define (parse-op-by-len ver input len)
  (letrec ([A (lambda (input pass res)
                (cond
                  [(= len pass)
                   (result 'op ver (reverse res) input)]
                  [else (let ([r (parse-packet input)]
                              [header (packet-header input)])
                          (A (result-remaining r)
                             (+ pass
                                (string-length header)
                                (body-len r))
                             (cons r res)))]))])
    (A input 0 '())))



(define (packet-body input)
  (substring input 6))

(define (body-len r)
  (if (literal-p r)
      (+ (length (result-body r))
         (for/fold ([total 0])
                   ([part (in-list (result-body r))])
           (+ total (string-length part))))
      (for/fold ([total 0])
                ([sr (in-list (result-body r))])
        (+ total (body-len sr)))))

(define (literal-p r)
  (symbol=? (result-type r)
            'literal))

(define (packet-header input)
  (substring input 0 6))

(define (packet-version input)
  (bits-to-decimal (substring (packet-header input) 0 3)))

(define (packet-type input)
  (let ([typ (substring (packet-header input) 3 6)])
    (if (string=? typ "100")
        'literal
        'op)))

(define (op-type input)
  (let ([c (string-ref input 0)])
    (if (char=? c #\0)
        'len
        'count)))

(define (op-len input)
  (let ([body (substring input 1)])
    (bits-to-decimal (substring body 0 15))))

(define (op-count input)
  (let ([body (op-body  input)])
    (bits-to-decimal (substring body 0 11))))

(define (op-body input typ)
  (if (symbol=? typ 'len)
      (substring input 16)
      (substring input 12)))

(define (op-header input)
  (let ([h (packet-header input)]
        [b (packet-body input)])
    (case (op-type b)
      [(len) (string-append h (substring b 0 16))]
      [(op) (string-append h (substring b 0 12))])))

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
  (result-body (parse-packet "110100101111111000101000"))
                                    ;; 1101000101001010010001001000000000
  (result-body (parse-packet "00111000000000000110111101000101001010010001001000000000"))
  )
