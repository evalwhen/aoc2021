(defpackage :aoc2021.day2
  (:use :cl)
  (:export :puzzle1
           :puzzle2)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int))

(in-package :aoc2021.day2)

(defparameter +depth+ 0)
(defparameter +distance+ 0)
(defparameter +aim+ 0)

;; oop version
;; TODO: functionnal version
(defclass command ()
  ((value
    :initarg :value
    :accessor value)))

(defclass up-command (command) ())
(defclass down-command (command) ())
(defclass forward-command (command) ())

(defclass up-command2 (command) ())
(defclass down-command2 (command) ())
(defclass forward-command2 (command) ())

(defgeneric execute (obj))

;; puzzle1
(defmethod execute ((obj up-command))
  (setf +depth+ (- +depth+
                   (value obj))))

(defmethod execute ((obj down-command))
  (setf +depth+ (+ (value obj)
                   +depth+)))

(defmethod execute ((obj forward-command))
  (setf +distance+ (+ (value obj)
                      +distance+)))

;; puzzle2
(defmethod execute ((obj up-command2))
  (setf +aim+ (- +aim+
                 (value obj))))

(defmethod execute ((obj down-command2))
  (setf +aim+ (+ (value obj)
                   +aim+)))

(defmethod execute ((obj forward-command2))
  (setf +distance+ (+ (value obj)
                      +distance+))
  (setf +depth+ (+ (* +aim+ (value obj))
                   +depth+)))


(defun parse-command (line)
  (let* ((idx (search " " line))
         (name (subseq line 0 idx))
         (value (parse-int (subseq line (incf idx)))))
    (cond
      ((string= name "up") (make-instance 'up-command :value value))
      ((string= name "down") (make-instance 'down-command :value value))
      ((string= name "forward") (make-instance 'forward-command :value value)))))

(defun parse-command2 (line)
  (let* ((idx (search " " line))
         (name (subseq line 0 idx))
         (value (parse-int (subseq line (incf idx)))))
    (cond
      ((string= name "up") (make-instance 'up-command2 :value value))
      ((string= name "down") (make-instance 'down-command2 :value value))
      ((string= name "forward") (make-instance 'forward-command2 :value value)))))


(defun execute-commands (commands)
  (cond
    ((null commands) nil)
    (t (progn
         (execute (car commands))
         (execute-commands (cdr commands))))))

(defun puzzle1 ()
  (let* ((commands (parse-input "input1.txt" #'parse-command)))
    (execute-commands commands)
    (* +depth+
       +distance+)))

(defun puzzle2 ()
  (let* ((commands (parse-input "input1.txt" #'parse-command2)))
    (execute-commands commands)
    (* +depth+
       +distance+)))
