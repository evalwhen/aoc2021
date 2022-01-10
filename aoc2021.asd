(defsystem "aoc2021"
  :version "0.1.0"
  :author "melp"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:module "day1"
                  :components
                  ((:file "main"))))))
  :description "advent of code 2021"
  :in-order-to ((test-op (test-op "aoc2021/tests"))))

(defsystem "aoc2021/tests"
  :author "melp"
  :license ""
  :depends-on ("aoc2021"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for aoc2021"
  :perform (test-op (op c) (symbol-call :rove :run c)))
