(defpackage aoc2021.day1
  (:use :cl)
  (:export :main))

(in-package :aoc2021.day1)

;; (with-open-file (stream "/etc/passwd")
;;   (loop for line = (read-line stream nil 'foo)
;;    until (eq line 'foo)
;;    do (print line)))

(defun parse-input (filename)
  (with-open-file (in filename :direction :input)
    (let ((lst '()))
      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (setf lst (append lst (cons (parse-int line) nil))))
      lst)))

(defun count-incr (lst)
  (labels ((help (leader lst count)
             (cond
               ((null lst) count)
               ((< leader (car lst)) (help (car lst) (cdr lst) (+ count 1)))
               (t (help (car lst) (cdr lst) count)))))
    (if (null lst)
        0
        (help (car lst) (cdr lst) 0))))

(defun parse-int (str)
  (handler-case (parse-integer str)
    (sb-int:simple-parse-error () 0)))

(defun main ()
  (let ((input (parse-input "input1.txt")))
    (count-incr input)))
