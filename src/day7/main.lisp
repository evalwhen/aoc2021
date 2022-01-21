(defpackage :aoc2021.day7
  (:export :puzzle1)
  (:use :cl))

(in-package :aoc2021.day7)

(defun input-to-nums (line)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, line) ")")))

(defun fules (nums)
  (loop for i from 0 to (apply #'max nums)
        collect (cons i (apply #'+ (loop for num in nums
                                         collect (abs (- num i)))))))

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
