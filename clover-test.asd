#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#


#|
  To execute clover test,
  1. load clover-test      : (ql:quickload :clover-test)
  2. in-package clover-test: (in-package :clover-test)
  3. run tests             : (run)
|#

(defsystem "clover-test"
  :author "moratori"
  :license ""
  :depends-on ("clover" "1am")
  :components ((:module "tests"
                :components
                ((:file "clover"))))
  :description "Test system for clover")
