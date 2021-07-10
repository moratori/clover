(defpackage clover.tests.resolution
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.resolution
        :1am))
(in-package :clover.tests.resolution)



(test clover.tests.resolution.resolution.test1

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
                   (multiple-value-bind 
                       (a b resoluted)
                       (%resolution clause1 clause2)
                     (declare (ignore a b))
                     resoluted))
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


(test clover.tests.resolution.resolution.test2
      (let* ((clause1
               (clause (list (literal nil 'P (list (constant 'A )
                                                   (constant 'B )
                                                   (constant 'C )))))
               )
             (clause2
               (clause (list (literal nil 'P (list (vterm 'u)
                                                   (vterm 'z)
                                                   (vterm 'w)))
                             (literal t   'P (list (vterm 'y)
                                                   (vterm 'z)
                                                   (vterm 'v)))
                             (literal t   'P (list (vterm 'x)
                                                   (vterm 'y)
                                                   (vterm 'u)))
                             (literal t   'P (list (vterm 'x)
                                                   (vterm 'v)
                                                   (vterm 'w)))))
               )
             (cs
               (clause-set (list clause1 clause2) :linear))
             (ret
               (comprehensive-resolvent
                 cs
                 clause1
                 clause2
                 :linear
                 (lambda (x) :center)
                 (lambda (x) :resolvent)
                 (lambda (x) :resolvent))))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'y)
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'v)
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (constant 'B )
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'x)
                                                    (constant 'A )
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (constant 'C )
                                                    (vterm 'w)))))))
            ret
            :test #'clause-set=))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'y)
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'v)
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (constant 'C )
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (constant 'B )
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (constant 'A )
                                                    (vterm 'v)
                                                    (vterm 'w)))))))
            ret
            :test #'clause-set=))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'y)
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'v)
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (constant 'C )))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (constant 'B )))
                              (literal t   'P (list (constant 'A )
                                                    (vterm 'y)
                                                    (vterm 'u)))))))
            ret
            :test #'clause-set=))))

