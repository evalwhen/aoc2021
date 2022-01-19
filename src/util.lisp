(defpackage aoc2021.util
  (:use :cl)
  (:export :parse-input
           :parse-int
           :get-tokens))

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

(defun get-tokens (line seprator &key (convert #'(lambda (x) x)))
  (labels ((get-word (start end)
             (cond
               ((<= (length line) end) (values (subseq line start end) end))
               ((char= (aref line end) seprator) (values (subseq line start end) (incf end)))
               (t (get-word start (incf end)))))
           (get-words (start res)
             (multiple-value-bind (word new-start) (get-word start start)
               (cond
                 ((string= word "") (get-words new-start res))
                 ((<= (length line) new-start) (append res (cons (funcall convert word) nil)))
                 (t (get-words new-start (append res (cons (funcall convert word) nil))))))))
    (get-words 0 nil)))
