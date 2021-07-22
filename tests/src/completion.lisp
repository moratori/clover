(defpackage clover.tests.completion
  (:use :cl
        :clover.types
        :clover.util
        :clover.completion
        :clover.rename
        :1am)
  (:import-from :clover.property
                :*term-order-algorithm*)
  )
(in-package :clover.tests.completion)



(test clover.tests.completion.kb-completion.test1
      (setf *term-order-algorithm* :original)

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (vterm 'x)
                    (fterm 'plus (list (constant 'ZERO) (vterm 'x))))
                  (equation
                    nil
                    (constant 'ZERO)
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x))))
                  (equation 
                    nil
                    (fterm 'plus (list (vterm 'x)
                                       (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                    (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                       (vterm 'z)))))))
            (result
              (kb-completion target 100)))
        (print (rename-for-human-readable-printing result))
        (is (and result (not (null (rewrite-rule-set.rewrite-rules result)))))))
