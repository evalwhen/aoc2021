(defpackage :aoc2021.day3
  (:use :cl)
  (:export :puzzle1
           :puzzle2)
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

;; TODO: refactor code.
(defun puzzle1 ()
  (parse-input "input1.txt" #'parse-line)
  (multiple-value-bind (g e) (cal-gamma-and-eplison)
    (* g e)))


(defun split (lines i)
  (let ((counter (make-hash-table)))
    (dolist (line lines)
      (setf (gethash (aref line i) counter)
            (append (gethash (aref line i) counter)
                    (cons line nil))))
    (let* ((l0 (gethash #\0 counter))
           (l1 (gethash #\1 counter))
           (l0-len (list-length l0))
           (l1-len (list-length l1)))
      (cond
        ((<= l0-len l1-len) (values l1 l0))
        (t (values l0 l1))))))

(defun cal-oxygen-rate (lines)
  (cond
    ((null (cdr lines)) (car lines))
    (t (labels ((help (ls i)
                      (multiple-value-bind (ol) (split ls i)
                        (if (null (cdr ol))
                            (car ol)
                            (help ol (+ i 1))))))
         (help lines 0)))))

(defun cal-c02-rate (lines)
  (cond
    ((null (cdr lines)) (car lines))
    (t (labels ((help (ls i)
                      (multiple-value-bind (ol cl) (split ls i)
                        (if (null (cdr cl)) ;; why (cdr nil) equal nil
                            (car cl)
                            (help cl (+ i 1))))))
         (help lines 0)))))

(defun puzzle2 ()
  (let* ((lines (parse-input "input1.txt" #'(lambda (x) x)))
         (orate-b (cal-oxygen-rate lines))
         (crate-b (cal-c02-rate lines))
         (orate (if (null orate-b) 0 (parse-integer orate-b :radix 2)))
         (crate (if (null crate-b) 0 (parse-integer crate-b :radix 2))))
    (* orate crate)))
