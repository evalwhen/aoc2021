(defpackage :aoc2021.day15
  (:use :cl)
  (:import-from :aoc2021.util
   :parse-input))

(in-package :aoc2021.day15)

;; (define (min-path data start end)
;;   (define m (- (sqrt (hash-count data)) 1))
;;   (define/memo (find-min start)
;;     (if (equal? start end)
;;       (risk-level data end)
;;       (for/fold ([m 1000000000000000])
;;                 ([neibor (neibors start m)]
;;                  #:when (hash-has-key? data neibor))
;;         (min m (+ (risk-level data start)
;;                   (find-min neibor))))
;;       ))
;;   (find-min start))

(defun neibors (pos m)
  (let* ((x (car pos))
         (y (cdr pos))
         (raw (list (cons x (incf y))
                    (cons (incf x)  y))))

    (remove-if-not #'(lambda (pos)
                       (and (<= 0 (car pos) m)
                            (<= 0 (cdr pos) m)))
                   raw)))


(defun min-path (data start end)
  (labels ((find-min (start)
             (if (equal? start end)
                 (aref data (car end) (cdr end))
                 (let ((nbs (neibors start (array-rank data))))
                   (if (= (length nbs) 2)
                       (+ (aref data (car start) (cdr start))
                          (min (find-min (car nbs))
                               (find-min (cdr nsb))))
                       (+ (aref data (car start) (cdr start))
                          (find-min (car nbs))))))))
    (find-min start)))
