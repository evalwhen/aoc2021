(in-package :cl-user)

(defpackage aoc2021-util-asd
  (:use :cl :asdf))
(in-package :aoc2021-util-asd)

(defsystem "aoc2021-util"
  :version "0.1.0"
  :author "melp"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "util"))))
  :description "advent of code 2021 utils"
  :in-order-to ((test-op (test-op "aoc2021/tests"))))

(register-system-packages "aoc2021-util" '(:aoc2021.util))
