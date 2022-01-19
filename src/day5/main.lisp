(defpackage :aoc2021.day5
  (:use :cl)
  (:export #:puzzle1
           #:puzzle2)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int))

(in-package :aoc2021.day5)

(defparameter *delim* "->")

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

(defun is-diagonal (seg)
  (= (abs (- (segment-x1 seg) (segment-x2 seg)))
     (abs (- (segment-y1 seg) (segment-y2 seg)))))

(defun gen-seq (s e)
  (let ((delt (if (< s e) 1 -1)))
    (labels ((help (s e lst)
               (cond
                 ((= s e) (reverse (cons e lst)))
                 (t (help (+ s delt)
                          e
                          (cons s lst))))))
      (if (= s e)
          s
          (help s e nil)))))

(defun gen-dots (seg)
  (let* ((xseq (gen-seq (segment-x1 seg) (segment-x2 seg)))
         (yseq (gen-seq (segment-y1 seg) (segment-y2 seg))))
    (labels ((help (xs ys lst)
               (cond
                 ((and (listp xs) (listp ys))
                  (cond ((null xs) lst)
                        (t (help (cdr xs)
                                 (cdr ys)
                                 (append lst (cons (cons (car xs) (car ys))
                                                   nil))))))
                 ((and (listp xs) (numberp ys))
                  (cond ((null xs) lst)
                        (t (help (cdr xs)
                                 ys
                                 (append lst (cons (cons (car xs) ys)
                                                   nil))))))
                 ((and (numberp xs) (listp ys))
                  (cond ((null ys) lst)
                        (t (help xs
                                  (cdr ys)
                                  (append lst (cons (cons xs (car ys))
                                                    nil)))))))))
      (help xseq yseq nil))))

(defun draw (seg map)
  (let ((dots (gen-dots seg)))
    (loop for dot in dots do
          (incf (aref map (car dot) (cdr dot))))))

(defun count-dangerous (map)
  (let ((counter 0))
    (loop for y from 0 below 1000 do
      (loop for x from 0 below 1000 do
        (when (>= (aref map y x) 2)
          (incf counter))))
    counter))

(defun puzzle1 ()
  (let* ((map (make-array '(1000 1000) :element-type 'integer))
         (segments (parse-input "input2.txt" #'parse-line))
         (input (remove-if #'(lambda (seg) (not (or (is-horizontal seg)
                                                    (is-vertical seg))))
                           segments)))
    (loop for seg in input do
      (draw seg map))
    (count-dangerous map)))

(defun puzzle2 ()
  (let* ((map (make-array '(1000 1000) :element-type 'integer))
         (segments (parse-input "input2.txt" #'parse-line))
         (input (remove-if #'(lambda (seg) (not (or (is-horizontal seg)
                                                    (is-vertical seg)
                                                    (is-diagonal seg))))
                           segments)))
    (loop for seg in input do
      (draw seg map))
    (count-dangerous map)))
