(defpackage aoc2021/tests/main
  (:use :cl
        :aoc2021
        :rove))
(in-package :aoc2021/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :aoc2021)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
