(defpackage clover.tests.resolution
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.resolution
        :1am))
(in-package :clover.tests.resolution)



(test clover.tests.resolution.resolution

      (is (let* ((clause1
                   (clause 
                     (list
                       (literal nil 'P (list (vterm 'x) (vterm 'y)))
                       (literal nil 'Q (list (vterm 'x))))))
                 (clause2
                   (clause 
                     (list
                       (literal t 'P (list (vterm 'z) (vterm 'z)))
                       (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                       (literal t 'Q (list (vterm 'w))))))
                 (res 
                   (%resolution clause1 clause2))
                 (expected
                   (list 
                     (clause 
                       (list 
                         (literal nil 'Q (list (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                         (literal t 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal t 'Q (list (vterm 'w)))
                         (literal t 'P (list (vterm 'z) (vterm 'z)))
                         (literal nil 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal nil 'P (list (vterm 'w) (vterm 'y))) 
                         (literal t 'P (list (vterm 'z) (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w))))))))))
            (every 
              (lambda (r)
                (member r expected
                  :test #'clause=))
              res)))
  )


