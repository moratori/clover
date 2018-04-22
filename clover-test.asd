#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#

(defsystem "clover-test"
  :defsystem-depends-on ("prove-asdf")
  :author "moratori"
  :license ""
  :depends-on ("clover"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "clover"))))
  :description "Test system for clover"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
