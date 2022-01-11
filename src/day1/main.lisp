(defpackage aoc2021.day1
  (:use :cl)
  (:export :puzzle1
   :puzzle2)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int))

(in-package :aoc2021.day1)

;; (defun parse-input (filename)
;;   (with-open-file (in filename :direction :input)
;;     (let ((lst '()))
;;       (loop for line = (read-line in nil :eof)
;;             until (eq line :eof)
;;             do (setf lst
;;                      (append lst
;;                              (cons (parse-int line) nil))))
;;       lst)))

(defun count-incr (lst)
  (labels ((help (leader lst count)
             (cond
               ((null lst) count)
               ((< leader (car lst)) (help (car lst) (cdr lst) (+ count 1)))
               (t (help (car lst) (cdr lst) count)))))
    (if (null lst)
        0
        (help (car lst) (cdr lst) 0))))

(defun group-triple (lst)
  (labels ((get-triple (lst count res)
             (cond
               ((= count 0) res)
               (t (get-triple (cdr lst)
                              (- count 1)
                              (append res
                                      (cons (car lst) nil))))))
           (collect (lst res)
             (cond
               ((< (list-length lst) 3) res)
               (t (collect (cdr lst) (append res (cons (get-triple lst 3 nil) nil)))))))
    (collect lst nil)))

(defun sum-triple (lst)
  (labels ((sum (lst res)
             (cond
               ((null lst) res)
               (t (sum (cdr lst) (+ (car lst) res)))))
           (help (lsts res)
             (cond
               ((null lsts) res)
               (t (help (cdr lsts)
                        (append res
                                (cons (sum (car lsts) 0)
                                      nil)))))))
    (help lst nil)))

(defun puzzle1 ()
  (let ((input (parse-input "input1.txt" #'parse-int)))
    (count-incr input)))

(defun puzzle2 ()
  (let* ((input (parse-input "input2.txt" #'parse-int))
         (triples (sum-triple (group-triple input))))
    (count-incr triples)))
