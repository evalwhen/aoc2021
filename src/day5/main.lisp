(defpackage :aoc2021.day5
  (:use :cl)
  (:export #:puzzle1)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int))

(in-package :aoc2021.day5)

(defparameter *delim* "->")
(defparameter *map* (make-array '(1000 1000) :element-type 'integer))

(defstruct segment x1 y1 x2 y2)

(defun get-tokens (line)
  (labels ((get-word (start end)
             (cond
               ((<= (length line) end) (values (subseq line start end) end))
               ((char= (aref line end) #\Space) (values (subseq line start end) (incf end)))
               (t (get-word start (incf end)))))
           (get-words (start res)
             (multiple-value-bind (word new-start) (get-word start start)
               (cond
                 ((string= word "") (get-words new-start res))
                 ((<= (length line) new-start) (append res (cons word nil)))
                 (t (get-words new-start (append res (cons word nil))))))))
    (get-words 0 nil)))

(defun parse-line (line)
  (let* ((toks (get-tokens line)))
    (cond
      ((= (length toks) 3)
       (let* ((start (car toks))
              (end (caddr toks))
              (start-point (str:split "," start))
              (end-point (str:split "," end)))
         (make-segment :x1 (parse-int (car start-point))
                       :y1 (parse-int (cadr start-point))
                       :x2 (parse-int (car end-point))
                       :y2 (parse-int (cadr end-point))))))))

(defun is-horizontal (seg)
  (= (segment-y1 seg) (segment-y2 seg)))

(defun is-vertical (seg)
  (= (segment-x1 seg) (segment-x2 seg)))

(defun start-to-end (i j)
  (if (< i j)
      (values i j)
      (values j i)))

(defun draw (seg)
  (if (is-horizontal seg)
      (multiple-value-bind (start end) (start-to-end (segment-x1 seg) (segment-x2 seg))
        (loop for x from start below (+ end 1) do
          (incf (aref *map* (segment-y1 seg) x))))
      (multiple-value-bind (start end) (start-to-end (segment-y1 seg) (segment-y2 seg))
        (loop for y from start below (+ end 1) do
              (incf (aref *map* y (segment-x1 seg)))))))

(defun count-dangerous ()
  (let ((counter 0))
    (loop for y from 0 below 1000 do
      (loop for x from 0 below 1000 do
        (when (>= (aref *map* y x) 2)
          (incf counter))))
    counter))

(defun puzzle1 ()
  (let* ((segments (parse-input "input2.txt" #'parse-line))
         (input (remove-if #'(lambda (seg) (not (or (is-horizontal seg)
                                                    (is-vertical seg))))
                           segments)))
    (loop for seg in input do
      (draw seg))
    (count-dangerous)))
