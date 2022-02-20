#lang racket

(define digits '(("0" . "0000")
                 ("1" . "0001")
                 ("2" . "0010")
                 ("3" . "0011")
                 ("4" . "0100")
                 ("5" . "0101")
                 ("6" . "0110")
                 ("7" . "0111")
                 ("8" . "1000")
                 ("9" . "1001")
                 ("A" . "1010")
                 ("B" . "1011")
                 ("C" . "1100")
                 ("D" . "1101")
                 ("E" . "1110")
                 ("F" . "1111")))

(define LITERAL-HEADER-LEN 6)
(define OP-LEN-HEADER-LEN (+ 6 1 15))
(define OP-CNT-HEADER-LEN (+ 6 1 11))

(define (digit-to-bin d)
  (cdr (assoc d digits)))

(define (input-to-bin in)
  (for/fold ([bin ""])
            ([d (in-string in)])
    (string-append bin (digit-to-bin (string d)))))

;; (module+ test
;;   (input-to-bin "D2FE28"))

(struct result (type ver body remaining) #:transparent)

(define (parse-packet input)
  (case (packet-type input)
    [(literal) (parse-literal (packet-version input)
                              (packet-body input))]
    [(op) (parse-op (packet-version input) (packet-body input))]))

(define (parse-op ver input)
  (case (op-type input)
    [(len) (parse-op-by-len ver (op-body input 'len) (op-len input))]
    [(cnt) (parse-op-by-count ver (op-body input 'cnt) (op-count input))]))

(define (parse-literal ver input)
  (define  (stop-p group)
    (char=? (string-ref group 0) #\0))

  (let loop ([in input] [out '()])
    (let* ([group (substring in 0 5)]
           [part (substring group 1)])
      (cond
        [(stop-p group) (result 'literal ver (reverse (cons part out)) (substring in 5))]
        [else (loop (substring in 5)
                    (cons part out))]))))

(define (parse-op-by-len ver input len)
  (letrec ([A (lambda (input pass res)
                (cond
                  [(= len pass)
                   (result 'op-len ver (reverse res) input)]
                  [else (let ([r (parse-packet input)])
                          (A (result-remaining r)
                             (+ pass
                                (current-len r))
                             (cons r res)))]))])
    (A input 0 '())))

(define (parse-op-by-count ver input cnt)
  (letrec ([A (lambda (input pass res)
                (cond
                  [(= cnt pass)
                   (result 'op-cnt ver (reverse res) input)]
                  [else (let ([r (parse-packet input)])
                          (A (result-remaining r)
                             (add1 pass)
                             (cons r res)))]))])
    (A input 0 '())))


(define (packet-body input)
  (substring input 6))

(define (current-len r)
  (cond [(literal-p r)
         (+ LITERAL-HEADER-LEN
            (length (result-body r))
            (for/fold ([total 0])
                      ([part (in-list (result-body r))])
              (+ total (string-length part))))]
        [(op-len-p r)
         (+ OP-LEN-HEADER-LEN
            (for/fold ([total 0])
                      ([sr (in-list (result-body r))])
              (+ total (current-len sr))))]
        [(op-cnt-p r)
         (+ OP-CNT-HEADER-LEN
            (for/fold ([total 0])
                      ([sr (in-list (result-body r))])
              (+ total (current-len sr))))]))

(define (version-sum r)
  (if (literal-p r)
      (result-ver r)
      (+ (result-ver r) (for/fold ([total 0])
                                  ([part (in-list (result-body r))])
                          (+ total (version-sum part))))))

(define (literal-p r)
  (symbol=? (result-type r)
            'literal))

(define (op-len-p r)
  (symbol=? (result-type r)
            'op-len))

(define (op-cnt-p r)
  (symbol=? (result-type r)
            'op-cnt))

(define (packet-header input)
  (substring input 0 6))

(define (packet-version input)
  (bits-to-decimal (substring (packet-header input) 0 3)))

(define (packet-type input)
  (let ([typ (substring (packet-header input) 3 6)])
    (cond
      [(string=? typ "100") 'literal]
      [else 'op])))

(define (op-type input)
  (let ([c (string-ref input 0)])
    (if (char=? c #\0)
        'len
        'cnt)))

(define (op-len input)
  (let ([body (substring input 1)])
    (bits-to-decimal (substring body 0 15))))

(define (op-count input)
  (let ([body (substring input 1)])
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
  (version-sum (parse-packet (input-to-bin "8A004A801A8002F478")))
  (version-sum (parse-packet (input-to-bin "620080001611562C8802118E34")))
  )

(define (puzzle1 filename)
  (define raw (call-with-input-file filename
                (lambda (in)
                  (read-line in))))
  (version-sum (parse-packet (input-to-bin raw)))
  )

(module+ test
  (puzzle1 "input1.txt")
  ;; (bits-to-decimal "0000000001000010")
  ;; (parse-packet "10100100000000010000100111111000000000100001001100001001010100011100111001111110111000111000001000000000010001000111100000")
  )
