(defpackage :aoc2021.day6.puzzle2
  (:use :cl)
  (:export :puzzle2)
  (:import-from :aoc2021.util
                :get-tokens
                :parse-int))

(in-package :aoc2021.day6.puzzle2)

(defun compact (fish)
  (loop for i from 0 to 8
        collect (count i fish)))

(defun pass-day (fish)
  (let* ((n (car fish))
         (new-fish (append (cdr fish) (list 0))))
    (incf (elt new-fish 6) n)
    (incf (elt new-fish 8) n)
    new-fish))

(defun tick (fish days)
  (cond
    ((= days 0) fish)
    (t (tick (pass-day fish) (- days 1)))))

(defun puzzle2 (days)
  (with-open-file (in "input1.txt" :direction :input)
    (let* ((line (read-line in))
           (input (get-tokens line #\, :convert #'parse-int))
           (fish (compact input)))
      (reduce #'+ (tick fish days)))))
