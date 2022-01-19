(defpackage :aoc2021.day6
  (:use :cl)
  (:export :puzzle1)
  (:import-from :aoc2021.util
                :get-tokens
                :parse-int))

(in-package :aoc2021.day6)

(defparameter *counter* 0)

(defun tick (lst days)
  (labels ((help (lst new-lst counter)
             (cond
               ((null lst) (values (reverse new-lst) counter))
               (t (help (cdr lst)
                        (if (> (car lst) 0)
                            (cons (- (car lst) 1) new-lst)
                            (cons 6 new-lst))
                        (if (= (car lst) 0)
                            (incf counter)
                            counter)))))
           (new (lst n)
             (cond
               ((= n 0) lst)
               (t (new (append lst (cons 8 nil))
                       (- n 1))))))
    (if (= days 0)
        lst
        (multiple-value-bind (nl c) (help lst nil 0)
          (v:info :counter c)
          (v:info :day days)
          (setf *counter* (+ *counter* c))
          (let ((new-lst (new nl c)))
            (tick new-lst (- days 1)))))))

(defun puzzle1 (days)
  (with-open-file (in "input1.txt" :direction :input)
    (let* ((line (read-line in))
           (input (get-tokens line #\, :convert #'parse-int)))
      (tick input days)
      (+ (length input) *counter*))))
