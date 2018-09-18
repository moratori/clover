(defpackage clover.tests.resolution
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.resolution
        :1am))
(in-package :clover.tests.resolution)


(test clover.tests.resolution.%collect-resolutable-literal

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
                       (literal t 'Q (list (vterm 'w)))))))
            (every 
              (lambda (x)
                (member 
                  x
                  (list 
                    (resolution-candidate
                      (literal nil 'P (list (vterm 'x) (vterm 'y)))
                      (list 
                        (unifier-set (list (unifier (vterm 'x) (vterm 'z))
                                           (unifier (vterm 'y) (vterm 'z))))
                        (unifier-set (list (unifier (vterm 'x) (vterm 'w))
                                           (unifier (vterm 'y) (fterm 'f (list (vterm 'w))))))))
                    (resolution-candidate
                      (literal nil 'Q (list (vterm 'x)))
                      (list 
                        (unifier-set (list (unifier (vterm 'x) (vterm 'w)))))))
                  :test #'resolution-candidate=))
              (%collect-resolutable-literal clause1 clause2))))
      

      )


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
                   (resolution clause1 clause2))
                 (expected
                   (list 
                     (clause 
                       (list 
                         (literal nil 'Q (list (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                         (literal t 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal t 'P (list (vterm 'z) (vterm 'z)))))
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


