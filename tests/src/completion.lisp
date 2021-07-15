(defpackage clover.tests.completion
  (:use :cl
        :clover.types
        :clover.util
        :clover.completion
        :1am))
(in-package :clover.tests.completion)



(test clover.tests.completion.kb-completion.test1
      (setf *term-order-algorithm* :original)
      (is t)
      )
