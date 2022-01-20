(defpackage :aoc2021.day6
  (:use :cl)
  (:export :puzzle1)
  (:import-from :aoc2021.util
                :get-tokens
                :parse-int))

(in-package :aoc2021.day6)

(defparameter *counter* 0)

(defun tick (lst days)
  (labels ((help (lst new-lst)
             (cond
               ((null lst) new-lst)
               (t (help (cdr lst)
                        (if (> (car lst) 0)
                            (cons (- (car lst) 1) new-lst)
                            (cons 8 (cons 6 new-lst))))))))
    (if (= days 0)
        lst
        (tick (help lst nil) (decf days))
        )))

(defun puzzle1 (days)
  (setf *counter* 0)
  (with-open-file (in "input1.txt" :direction :input)
    (let* ((line (read-line in))
           (input (get-tokens line #\, :convert #'parse-int)))
      (length (tick input days))
      )))
