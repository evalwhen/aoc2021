(defpackage :aoc2021.day3
  (:use :cl)
  (:export :puzzle1)
  (:import-from :aoc2021.util
   :parse-input
                :parse-int))

(in-package :aoc2021.day3)

(defparameter *colums* (make-hash-table))

(defun parse-line (line)
  (loop with i = 0
        for char across line
        do (progn
             (setf (gethash i *colums*)
                   (append (gethash i *colums*)
                           (cons char nil)))
             (incf i))))

(defun cal-most-and-least-value (lst)
  (let ((counter (make-hash-table :size 2)))
    (loop for char across "10"
          do (setf (gethash char counter) 0))
    (loop for char in lst
          do (setf (gethash char counter)
                   (+ (gethash char counter) 1)))
    (if (> (gethash #\1 counter)
           (gethash #\0 counter))
        (values #\1 #\0)
        (values #\0 #\1))))

(defun cal-gamma-and-eplison ()
  (with-hash-table-iterator (my-iterator *colums*)
    (let ((gamma-lst nil)
          (eplison-lst nil))
      (loop
        (multiple-value-bind (entry-p key value) ;; TODO suppress warning: key is unused
            (my-iterator)
          (if entry-p
              (multiple-value-bind (g e) (cal-most-and-least-value value)
                (setf gamma-lst (append gamma-lst (cons g nil)))
                (setf eplison-lst (append eplison-lst (cons e nil))))
              (return))))
      (values (parse-integer (list-to-string gamma-lst) :radix 2)
              (parse-integer (list-to-string eplison-lst) :radix 2)))))

(defun list-to-string (lst)
  (let ((res (make-array 0
                         :element-type 'character
                         :fill-pointer 0
                         :adjustable t)))
    (dolist (char lst)
      (vector-push-extend char res))
    res))

(defun puzzle1 ()
  (parse-input "input1.txt" #'parse-line)
  (multiple-value-bind (g e) (cal-gamma-and-eplison)
    (* g e)))

