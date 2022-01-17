(defpackage aoc2021.util
  (:use :cl)
  (:export :parse-input
           :parse-int))

(in-package :aoc2021.util)

(defun parse-input (filename line-parser)
  (with-open-file (in filename :direction :input)
    (let ((lst '()))
      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (setf lst
                     (append lst
                             (cons (funcall line-parser line) nil))))
      lst)))

(defun parse-int (str)
  (handler-case (parse-integer str)
    (sb-int:simple-parse-error () 0)))

(defmacro define-function (name lambda-list &body body)
  `(progn
     (export ',name)
     (defun ,name ,lambda-list ,@body)))
