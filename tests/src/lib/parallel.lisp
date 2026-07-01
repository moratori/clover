(defpackage clover.tests.lib.parallel
  (:use :cl
        :clover.lib.parallel
        :1am)
  
  )
(in-package :clover.tests.lib.parallel)


(test clover.tests.lib.parallel.get-number-of-processors.test1
      (is (<= 1 (clover.lib.parallel::get-number-of-processors)))
      (is (<= 1 (clover.lib.parallel::get-number-of-processors)))
      )
