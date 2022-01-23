(in-package :aoc2021.day8)

(defvar *encode-digit* (make-hash-table :test 'equal))
(defvar *digit-encode* (make-hash-table))

(defparameter *unique-length* '((2 1)
                                (4 4)
                                (3 7)
                                (7 8)))

(defun find-1-4-7-8 (nums)
  (loop for num in nums do
    (alexandria:when-let ((val (assoc (length num) *unique-length*)))
      (v:debug :val val)
      (setf (gethash (sort  num #'char<) *encode-digit*) (second val))
      (setf (gethash (second val) *digit-encode*) num))))

(defun num-diff (origin target)
  (set-difference (coerce origin 'list)
                  (coerce target 'list)))

(defun n-xs (n xs)
  (loop for x in xs
        collect (cons (num-diff n x) x)))

(defun n9 (eight four length-6s)
  (let* ((8-4 (num-diff eight four))
         (n9 (remove-if-not #'(lambda (x) (member (car (car x)) 8-4 :test #'char=))
                            (n-xs eight length-6s))))
    (cdr (car n9))))

(defun n6 (eight seven length-6s)
  (let* ((8-7 (num-diff eight seven))
         (n6 (remove-if #'(lambda (x) (member (car (car x)) 8-7 :test #'char=))
                            (n-xs eight length-6s))))
    (cdr (car n6))))

(defun n0 (length-6s n6 n9)
  (car (set-difference length-6s
                       (list n6 n9))))

(defun n5 (six length-5s)
  (cdr (car (remove-if-not #'(lambda (x) (= (length (car  x)) 1))
                            (n-xs six length-5s)))))

(defun n2 (nine length-5s)
  (cdr (car (remove-if-not #'(lambda (x) (= (length (car  x)) 2))
                            (n-xs nine length-5s)))))

(defun n3 (length-5s n2 n5)
  (car (set-difference length-5s
                       (list n2 n5))))
(defun length-p (n)
  #'(lambda (num) (= (length num) n)))

(defun decode (nums targets)

  (let ((*encode-digit* (make-hash-table :test 'equal))
        (digit 0))
      (find-1-4-7-8 nums)
      (let* ((length-6s (remove-if-not (length-p 6) nums))
             (n9 (sort (n9 (gethash 8 *digit-encode*)
                           (gethash 4 *digit-encode*)
                           length-6s)
                       #'char<))
             (n6 (sort (n6 (gethash 8 *digit-encode*)
                           (gethash 7 *digit-encode*)
                           length-6s)
                       #'char<))
             (n0 (sort (n0 length-6s n6 n9)
                       #'char<))
             (length-5s (remove-if-not (length-p 5) nums))
             (n5 (sort (n5 n6 length-5s) #'char<))
             (n2 (sort (n2 n9 length-5s) #'char<))
             (n3 (sort (n3 length-5s n5 n2) #'char<))
             )
        (setf (gethash n9 *encode-digit*) 9)
        (setf (gethash n0 *encode-digit*) 0)
        (setf (gethash n6 *encode-digit*) 6)
        (setf (gethash n5 *encode-digit*) 5)
        (setf (gethash n2 *encode-digit*) 2)
        (setf (gethash n3 *encode-digit*) 3))

      (loop for tar in targets do
        (setf digit (+ (* digit 10) (gethash (sort tar #'char<) *encode-digit*))))
    digit))

(defun parse-line (line)
  (let* ((delimiter (search "|" line))
         (part1 (subseq line 0 delimiter))
         (part2 (subseq line (incf delimiter)))
         (samples (get-tokens part1 #\Space))
         (targets (get-tokens part2 #\Space)))
    (cons samples targets)))


(defun puzzle2 ()
  (let* ((lss (parse-input "input2.txt" #'parse-line)))
    (loop for ls in lss sum (decode (car ls) (cdr ls)))))
