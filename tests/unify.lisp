(defpackage clover.tests.unify
  (:use :cl
        :clover.unify
        :clover.types
        :1am))
(in-package :clover.tests.unify)



(test test.%collect-unifier-set-candidate 
      (is 
        (let ((us 
                (clover.unify::%collect-unifier-set-candidate 
                  (vterm 'x) (vterm 'y))))
          (print us)
          )
        )
      )
