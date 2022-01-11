(defpackage :aoc2021.day2
  (:use :cl)
  (:export :puzzle1)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int))

(in-package :aoc2021.day2)

(defparameter +depth+ 0)
(defparameter +distance+ 0)

(defclass command ()
  ((value
    :initarg :value
    :accessor value)))

(defclass up-command (command) ())
(defclass down-command (command) ())
(defclass forward-command (command) ())

(defgeneric execute (obj))

(defmethod execute ((obj up-command))
  (setf +depth+ (- +depth+
                   (value obj))))

(defmethod execute ((obj down-command))
  (setf +depth+ (+ (value obj)
                   +depth+)))

(defmethod execute ((obj forward-command))
  (setf +distance+ (+ (value obj)
                      +distance+)))

(defun parse-comand (line)
  (let* ((idx (search " " line))
         (name (subseq line 0 idx))
         (value (parse-int (subseq line (incf idx)))))
    (cond
      ((string= name "up") (make-instance 'up-command :value value))
      ((string= name "down") (make-instance 'down-command :value value))
      ((string= name "forward") (make-instance 'forward-command :value value)))))

(defun execute-commands (commands)
  (cond
    ((null commands) nil)
    (t (progn
         (execute (car commands))
         (execute-commands (cdr commands))))))

(defun puzzle1 ()
  (let* ((commands (parse-input "input1.txt" #'parse-comand)))
    (execute-commands commands)
    (* +depth+
       +distance+)))
