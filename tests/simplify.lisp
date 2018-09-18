(defpackage clover.tests.simplify
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.simplify
        :1am))
(in-package :clover.tests.simplify)


(test clover.tests.simplify.simplify

      (is 
        (let ((clause 
                (clause (list (literal t 'P nil)
                              (literal nil 'P nil))))
              (expected
                (clause nil)))
          (clause= (simplify clause) expected)))
      
      (is 
        (let ((clause 
                (clause (list (literal t 'P (list (vterm 'x)))
                              (literal nil 'P (list (vterm 'x))))))
              (expected
                (clause nil)))
          (clause= (simplify clause) expected)))
      
      (is 
        (let ((clause 
                (clause (list (literal t 'P (list (fterm 'f nil)))
                              (literal nil 'P (list (fterm 'f nil))))))
              (expected
                (clause nil)))
          (clause= (simplify clause) expected)))
      
      (is 
        (let*((clause1
                (clause (list (literal t 'P (list (fterm 'f nil)))
                              (literal nil 'P (list (fterm 'f nil))))))
              (clause2
                (clause (list (literal t 'P (list (vterm 'x)))
                              (literal nil 'P (list (vterm 'x))))))
              (clause-set
                (clause-set (list clause1 clause2)))
              (expected
                (clause-set (list (clause nil)))))
          (clause-set= (simplify clause-set) expected)))

      )
