(defpackage clover.tests.rename
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.rename
        :1am))
(in-package :clover.tests.rename)


(test clover.tests.rename.rename

      (is 
        (let ((clause 
                (clause (list (literal t 'P   (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'Q (list (fterm 'f (list (vterm 'x) (vterm 'y)))))
                              (literal nil 'P nil)))))
          (format t "~%~A~%" (rename clause))
          t))
      
      (is 
        (let ((clause 
                (clause (list (literal t 'P   (list (vterm 'x) (vterm 'y)))
                              (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'Q (list (fterm 'f (list (vterm 'x) (vterm 'y)))))
                              (literal nil 'P nil)))))
          (format t "~%~A~%" (rename clause))
          t))

      )
