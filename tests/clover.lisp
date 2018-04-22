(defpackage clover-test
  (:use :cl
        :clover
        :types.clover
        :prove))
(in-package :clover-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clover)' in your Lisp.

(plan 4)

(subtest "OBJ CONSTRUCTION:: primitive expr test"
        (ok (expr nil 'P '(x y z)))
        (ok (expr t 'P '(x y z)))
        (ok (expr nil 'P '(x)))
        (ok (expr t 'P '(x)))
        (ok (expr nil 'P '()))
        (ok (expr t 'P '()))
        )

(subtest "OBJ CONSTRUCTION:: expr-set test"
        (ok (expr-set nil))
        (ok (expr-set (list (expr t 'P '(x y z)))))
        (ok (expr-set (list (expr t 'P '(x y z))
                            (expr nil 'P '(x y z))
                            (expr nil 'P '()))))
        (ok (expr-set (list (expr t 'Pred1 '(x y z))
                            (expr nil 'Pred2 '(x y z))
                            (expr nil 'Pred3 '()))))
        )

(subtest "OBJ CONSTRUCTION:: clause test"
        (ok (clause-set (list (expr-set nil) 
                              (expr-set (list (expr t 'P '(x y z))))
                              (expr-set (list (expr t 'Pred1 '(x y z))
                                              (expr nil 'Pred2 '(x y z))
                                              (expr nil 'Pred3 '()))))))
        )


(subtest "METHOD BEHAVIOR:: finish check"
         
        (ok (finish (clause-set (list (expr-set nil) 
                                      (expr-set (list (expr t 'P '(x y z))))
                                      (expr-set (list (expr t 'Pred1 '(x y z))
                                                      (expr nil 'Pred2 '(x y z))
                                                      (expr nil 'Pred3 '())))))))
        
        (ok (not (finish (clause-set (list (expr-set (list (expr t 'P '(x y z))))
                                           (expr-set (list (expr t 'Pred1 '(x y z))
                                                           (expr nil 'Pred2 '(x y z))
                                                           (expr nil 'Pred3 '()))))))))
        )


(finalize)





