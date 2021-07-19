(defpackage clover.tests.completion
  (:use :cl
        :clover.types
        :clover.util
        :clover.completion
        :1am))
(in-package :clover.tests.completion)



(test clover.tests.completion.kb-completion.test1
      (setf *term-order-algorithm* :original)
      (let ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (vterm 'x)
                    (fterm 'mult (list (constant 'ONE) (vterm 'x))))
                  (equation
                    nil
                    (constant 'ONE)
                    (fterm 'mult (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x))))
                  (equation 
                    nil
                    (fterm 'mult (list (vterm 'x)
                                       (fterm 'mult (list (vterm 'y) (vterm 'z)))))
                    (fterm 'mult (list (fterm 'mult (list (vterm 'x) (vterm 'y))) 
                                       (vterm 'z))))))))
        (is (kb-completion target 20))))
