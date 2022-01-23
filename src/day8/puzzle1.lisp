(defpackage :aoc2021.day8
  (:use :cl)
  (:import-from :for
                :for)
  (:import-from :aoc2021.util
                :parse-input
                :get-tokens))

(in-package :aoc2021.day8)

(defparameter *sizes* '(2 4 3 7))

(defun count-digit (lst)
  (loop for ele in lst
        sum (if (find (length ele) *sizes*)
                1
                0)))

(defun parse-line (line)
  (let* ((delimiter (search "|" line))
         (target (subseq line (incf delimiter)))
         (toks (get-tokens target #\Space)))
    toks))

(defun puzzle1 ()
  (let* ((lsts (parse-input "input2.txt" #'parse-line)))
    (loop for lst in lsts
          sum (count-digit lst))))
