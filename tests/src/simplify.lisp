(defpackage clover.tests.simplify
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.simplify
        :1am))
(in-package :clover.tests.simplify)



(test clover.tests.simplify.%remove-independent-clause

      (is 
        (let*((clause1
                (clause (list (literal nil 'P (list (fterm 'A nil)))
                              (literal nil 'P (list (fterm 'g (list (vterm 'x))))))))
              (clause2
                (clause (list (literal nil 'P (list (vterm 'x)))
                              (literal nil 'P (list (fterm 'h nil))))))
              (clauses
                (list clause1 clause2))
              (expected
                (clause-set nil)))
          (clause-set= (clause-set (clover.simplify::%remove-independent-clause clauses))
                       expected)))
      
      (is 
        (let*((clause1
                (clause (list (literal t 'P (list (fterm 'A nil)))
                              (literal nil 'P (list (fterm 'g (list (vterm 'x))))))))
              (clause2
                (clause (list (literal nil 'Q (list (vterm 'x)))
                              (literal nil 'P (list (fterm 'h nil))))))
              (clauses
                (list clause1 clause2))
              (expected
                (clause-set (list 
                              (clause (list (literal t 'P (list (fterm 'A nil)))
                                            (literal nil 'P (list (fterm 'g (list (vterm 'x)))))))))))
          (clause-set= (clause-set (clover.simplify::%remove-independent-clause clauses))
                       expected)))
      
      (is 
        (let*((clause1
                (clause (list (literal t 'P (list (fterm 'A nil)))
                              (literal t 'P (list (fterm 'g (list (vterm 'x))))))))
              (clause2
                (clause (list (literal t 'P (list (vterm 'x)))
                              (literal t 'P (list (fterm 'h nil))))))
              (clauses
                (list clause1 clause2))
              (expected
                (clause-set nil)))
          (clause-set= (clause-set (clover.simplify::%remove-independent-clause clauses))
                       expected)))
      )


(test clover.tests.simplify.simplify

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
                (clause-set nil )))
          (clause-set= (simplify clause-set) expected)))

      )
