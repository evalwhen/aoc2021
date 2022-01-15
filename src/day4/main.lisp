(defpackage :aoc2021.day4
  (:use :cl)
  (:export :puzzle1)
  (:import-from :aoc2021.util
                :parse-int))

(in-package :aoc2021.day4)

(defparameter board-size 5)
(defparameter win-status "11111")

(defstruct board content status)

(defun init-status ()
  (make-array (list board-size board-size) :element-type 'character :initial-element #\0))

(defun mark (b n)
  (let ((r -1)
        (c -1))
    (block mark-body
      (loop for i from 0 below board-size do
        (loop for j from 0 below board-size do
          (when (string= n (aref (board-content b) i j))
            (setf (aref (board-status b) i j) #\1)
            (setf r i)
            (setf c j)
            (return-from mark-body)))))
    (values r c)))

(defun sum-unmarked (b)
  (let ((sum 0))
    (loop for i below board-size do
      (loop for j below board-size do
        (unless (char= #\1 (aref (board-status b) i j))
          (incf sum (parse-int (aref (board-content b) i j))))))
    sum))

(defun check-win (b i j)
  (flet ((get-row (a r typ)
           (let ((res (make-array board-size :element-type typ)))
             (loop for j from 0 below board-size do
               (setf (aref res j)
                     (aref a r j)))
             res))
         (get-column (a c typ)
           (let ((res (make-array board-size :element-type typ)))
             (loop for i from 0 below board-size do
               (setf (aref res i)
                     (aref a i c)))
             res)))
    (let ((row (get-row (board-status b) i 'character))
          (column (get-column (board-status b) j 'character)))
      (cond
        ((string= row win-status) (get-row (board-content b) i 'string ))
        ((string= column win-status) (get-column (board-content b) j 'string))
        (t nil)))))

(defun get-toks (filename)
  (with-open-file (in filename :direction :input)
    (let ((lst nil)
          (input (read-line in nil :eof))
          (board nil))

      (assert (string= "" (read-line in nil :eof)))

      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (progn (if (string= "" line)
                          (progn (setf lst
                                       (append lst (cons board nil)))
                                 (setf board nil))
                          (setf board
                                (append board (cons line nil))))))

      (values input (append lst (cons board nil))))))

(defun parse-board (lst)
  (let ((board (make-array (list board-size board-size) :element-type 'string))
        (i 0))
    (loop for row in lst do
      (loop for cel in (parse-line row) do
        (progn (unless (string= cel "")
                 (setf (row-major-aref board i) cel)
                 (incf i)))))
    board))

(defun parse-line (line)
  (str:split " " line))

(defun parse-input (filename)
  (let ((boards nil))
    (multiple-value-bind (input bds) (get-toks filename)
      (loop for bd in bds do
        (setf boards
              (append boards (cons (make-board :content
                                               (parse-board bd)
                                               :status
                                               (init-status))
                                   nil))))
      (values (str:split "," input) boards))))

(defun puzzle1 ()
  (multiple-value-bind (input boards) (parse-input "input2.txt")
    (loop for number in input do
      (loop for board in boards do
        (multiple-value-bind (i j) (mark board number)
          (unless (= i -1)
            (let ((nums (check-win board i j)))
              (unless (null nums)
                (v:info :nums nums)
                (v:info :sum-unkarked (sum-unmarked board))
                (v:info :num number)
                (v:info :board board)
                (return-from puzzle1 (* (parse-integer number)
                                         (sum-unmarked board)))))))))))
