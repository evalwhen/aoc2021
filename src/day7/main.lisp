(defpackage :aoc2021.day7
  (:export :puzzle1)
  (:use :cl))

(in-package :aoc2021.day7)

(defun input-to-nums (line)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, line) ")")))

(defun fules (nums &key (c #'(lambda (x) x)))
  (loop for i from 0 to (apply #'max nums)
        collect (cons i (apply #'+ (loop for num in nums
                                         collect (funcall c (abs (- num i))))))))

(defun step-to-cost (num)
  (loop for i from 1 to num sum i))

(defun least (fus)
  (let* ((fs (mapcar #'cdr fus))
         (m (apply #'min fs)))
    (remove-if-not
     #'(lambda (fu)
         (= m (cdr fu)))
     fus)))

(defun puzzle1 ()
  (with-open-file (in "input2.txt")
    (let* ((nums (input-to-nums (read-line in nil))))
      (least (fules nums)))))

(defun puzzle2 ()
  (with-open-file (in "input2.txt")
    (let* ((nums (input-to-nums (read-line in nil))))
      (least (fules nums :c #'step-to-cost)))))
