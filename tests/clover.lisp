(defpackage clover.tests.types
  (:use :cl
        :clover.core
        :clover.types
        :1am))
(in-package :clover.tests.types)



(test test-expr
        (is (expr nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))
        (is (expr t 'P   (list (vterm 'x) (vterm 'y) (vterm 'z))))
        (is (expr nil 'P (list (vterm 'x))))
        (is (expr t 'P (list (vterm 'x))))
        (is (expr nil 'P nil))
        (is (expr t 'P nil))
        )

(test test-clause
        (is (clause nil))
        (is (clause (list (expr t 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))))
        (is (clause (list (expr t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (expr nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (expr nil 'P nil))))
        (is (clause (list (expr t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (expr nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (expr nil 'Pred3 nil))))
        )

(test test-clause
        (is (clause-set (list (clause nil) 
                              (clause (list (expr t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                              (clause (list (expr t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (expr nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (expr nil 'Pred3 nil))))))
        )


(test test-finish
         
        (is (finish (clause-set (list (clause nil) 
                                      (clause (list (expr t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                      (clause (list (expr t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                      (expr nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                      (expr nil 'Pred3 nil)))))))
        
        (is (not (finish (clause-set (list (clause (list (expr t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                           (clause (list (expr t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                           (expr nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                           (expr nil 'Pred3 nil))))))))
        )

