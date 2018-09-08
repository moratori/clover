(defpackage clover.tests.clover
  (:use :cl
        :clover.clover
        :clover.types
        :1am))
(in-package :clover.tests.clover)



(test test.finish
         
        (is (finish 
              (clause-set (list (clause nil) 
                                (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred3 nil)))))))
        
        (is (not (finish (clause-set (list (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                           (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                         (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                         (literal nil 'Pred3 nil))))))))
        )
