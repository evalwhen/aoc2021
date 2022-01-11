(defpackage aoc2021.util
  (:use :cl))

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
