(defpackage clover.tests.clover
  (:use :cl
        :clover.clover
        :clover.types
        :clover.resolution
        :1am))
(in-package :clover.tests.clover)



(test clover.tests.clover.finish
         
        (is (clover.clover::finish 
              (clause-set (list (clause nil) 
                                (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred3 nil)))))))
        (is (clover.clover::finish 
              (clause-set (list  (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                 (clause nil)
                                 (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                               (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                               (literal nil 'Pred3 nil)))))))
        
        (is (not (clover.clover::finish (clause-set (list (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                                          (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                                        (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                                        (literal nil 'Pred3 nil))))))))
        )


(test clover.tests.clover.start_resolution

        (is (start_resolution
              (clause-set (list  (clause (list (literal t 'P nil)))
                                 (clause (list (literal nil 'P nil)))))))

        (is (start_resolution
              (clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                 (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                                 (clause (list (literal t 'P nil)))
                                 ))))
        
        )
