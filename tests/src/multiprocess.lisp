(defpackage clover.tests.multiprocess
  (:use :cl
        :clover.multiprocess
        :1am)
  
  )
(in-package :clover.tests.multiprocess)


(test clover.tests.multiprocess.get-number-of-processors.test1
      (is (<= 1 (clover.multiprocess::get-number-of-processors)))
      (is (<= 1 (clover.multiprocess::get-number-of-processors)))
      )
