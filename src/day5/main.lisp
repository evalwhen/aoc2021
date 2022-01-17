(defpackage :aoc2021.day5
  (:use :cl))

(in-package :aoc2021.day5)

(defparameter *delim* "->")
(defparameter *map* (make-array '(10 10) :element-type 'integer))

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
         (make-segment :x1 (car start-point)
                       :y1 (cadr start-point)
                       :x2 (car end-point)
                       :y2 (cadr end-point)))))))
