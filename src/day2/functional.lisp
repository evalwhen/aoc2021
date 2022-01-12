(defpackage aoc2021.day2.functional
  (:use :cl)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int))

(in-package :aoc2021.day2.functional)

(defparameter +depth+ 0)
(defparameter +distance+ 0)
(defparameter +aim+ 0)

(defun parse-command (line)
  (let* ((idx (search " " line))
         (name (subseq line 0 idx))
         (value (parse-int (subseq line (incrf idx)))))
    (cons name value)))

(defun execute-command-1 (cmd)
  (let ((name (car cmd))
        (amount (cdr cmd)))
    (cond
     ((string= "up" name) (decf +depth+ amount))
     ((string= "down" name) (incf +depth+ amount))
     ((string= "forward" name (incf +distance+ amount)))
     (t (error "unkown cmd" cmd)))))

(defun execute-command-2 (cmd)
  (let ((name (car cmd))
        (amount (cdr cmd)))
    (cond
     ((string= "up" name) (decf +aim+ amount))
     ((string= "down" name) (incf +aim+ amount))
     ((string= "forward" name (progn (incf +distance+ amount)
                                     (incf +depth+ (* amount +aim+)))))
     (t (error "unkown cmd" cmd)))))

(defun execute-commands (cmds exec)
  (cond
   ((null cmds) nil)
   (t (progn
        (funcall exec (car cmds))
        (execute-commands (cdr cmds) exec)))))

(defun puzzle1 ()
  (let* ((cmds (parse-input #'parse-command)))
    (execute-commands cmds #'execute-command-1)))

(defun puzzle1 ()
  (let* ((cmds (parse-input #'parse-command)))
    (execute-commands cmds #'execute-command-2)))
