(defpackage clover.tests.types
  (:use :cl
        :clover.types
        :1am))
(in-package :clover.tests.types)


(test test.vterm 
      (is (vterm 'x))
      (is (vterm 'x_y_z-a-b-c))
      )

(test test.fterm
      (is (fterm 'f (list (vterm 'x) (vterm 'y))))
      (is (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'z))))))
      (is (fterm 'h nil))
      )


(test test.literal
        (is (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))
        (is (literal t 'P   (list (vterm 'x) (vterm 'y) (vterm 'z))))
        (is (literal nil 'P (list (vterm 'x))))
        (is (literal t 'P (list (vterm 'x))))
        (is (literal nil 'P nil))
        (is (literal t 'P nil))
        )

(test test.clause
        (is (clause nil))
        (is (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))))
        (is (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'P nil))))
        (is (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'Pred3 nil))))
        )

(test test.clause-set
        (is (clause-set (list (clause nil) 
                              (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                              (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred3 nil))))))
        )
 
