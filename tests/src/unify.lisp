(defpackage clover.tests.unify
  (:use :cl
        :clover.conditions
        :clover.unify
        :clover.types
        :clover.util
        :1am))
(in-package :clover.tests.unify)



(test clover.tests.unify.%collect-disagreement-set 
      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (vterm 'x) (vterm 'y))))
          (unifier-set= 
            us
            (unifier-set 
              (list (unifier (vterm 'x) (vterm 'y)))))))

      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (vterm 'x) (fterm 'f (list (vterm 'z))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'f (list (vterm 'z)))))))))

      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (fterm 'f (list (vterm 'x))) (fterm 'f (list (vterm 'z))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'z)))))))
      
      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (fterm 'f (list (vterm 'x))) (fterm 'f (list (fterm 'g (list (vterm 'z))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'g (list (vterm 'z)))))))))
      
      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (fterm 'f (list (vterm 'x) 
                                  (vterm 'y))) 
                  (fterm 'f (list (fterm 'g (list (vterm 'z))) 
                                  (fterm 'h (list (vterm 'w))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'g (list (vterm 'z))))
                    (unifier (vterm 'y) (fterm 'h (list (vterm 'w)))))))))
       
      (is 
        (handler-case
            (clover.unify::%collect-disagreement-set 
              (vterm 'x) (fterm 'f (list (vterm 'x))))
            (occurrence-check-error (e)
              t)))
      
      (is 
        (handler-case
            (clover.unify::%collect-disagreement-set 
              (fterm 'f (list (vterm 'x))) (fterm 'f (list (vterm 'x))))
            (occurrence-check-error (e)
              t)))

      (is 
        (handler-case
            (clover.unify::%collect-disagreement-set 
              (fterm 'g (list (vterm 'x))) (fterm 'f (list (vterm 'z))))
            (unmatching-fterm-error (e)
              t)))

      )


(test clover.tests.unify.find-most-general-unifier-set

      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (literal nil 'P (list (vterm 'x) (vterm 'x)))
                  (literal t   'P (list (vterm 'y) (fterm 'f (list (vterm 'w))))))))

          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'f (list (vterm 'w))))
                    (unifier (vterm 'y) (fterm 'f (list (vterm 'w)))))))))
      
      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (literal nil 'P (list (vterm 'x) (vterm 'x) (vterm 'y)))
                  (literal t   'P (list (vterm 'w) (vterm 'v) (fterm 'f (list (vterm 'v))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'v))
                    (unifier (vterm 'y) (fterm 'f (list (vterm 'v))))
                    (unifier (vterm 'w) (vterm 'v)))))))
      
      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (literal nil 'P (list (vterm 'x) (vterm 'x)))
                  (literal t   'P (list (vterm 'w) (vterm 'v))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'v))
                    (unifier (vterm 'w) (vterm 'v)))))))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'Q (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-literal-error (e) t)))

      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal t 'P (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-literal-error (e) t)))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-literal-error (e) t)))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'P (list (vterm 'x) (vterm 'x)))
              (literal t   'P (list (fterm 'f (list (vterm 'w))) 
                                    (fterm 'g (list (vterm 'v))))))
          (ununifiable-literal-error (e) t)))

      )


(test clover.tests.unify.subsumption-clause-p.test1
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x)))))
              (clause (list (literal nil 'P 
                                     (list (fterm 'f (list (vterm 'y)))))
                            (literal t 'Q
                                     (list (vterm 'y) (vterm 'z)))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x)))))
              (clause (list (literal nil 'P 
                                     (list (fterm 'g (list (vterm 'y)))))
                            (literal nil 'Q
                                     (list (vterm 'y)))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))))
              (clause (list (literal nil 'P 
                                     (list (fterm 'A nil) (fterm 'B nil)))
                            (literal nil 'Q
                                     (list (fterm 'A nil) (fterm 'B nil)))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))))
              (clause (list (literal nil 'P 
                                     (list (fterm 'A nil) 
                                           (fterm 'f (list (vterm 'z)))))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))
                            (literal nil 'Q
                                     (list (vterm 'y) (vterm 'x)))))

              (clause (list (literal nil 'P 
                                     (list (fterm 'B nil) 
                                           (fterm 'f (list (vterm 'z)
                                                           (fterm 'g (list (vterm 'w)))))))
                            (literal nil 'Q
                                     (list (fterm 'f (list (vterm 'z)
                                                           (fterm 'g (list (vterm 'w)))))
                                           (fterm 'B nil)))
                            (literal t 'R 
                                     (list (fterm 'A nil)
                                           (fterm 'f (list (vterm 'z))))))))))

(test clover.tests.unify.subsumption-clause-p.test2
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (vterm 'x)))
                               (literal nil 'Q
                                        (list (vterm 'x) (vterm 'y)))))
                 (clause (list (literal nil 'P
                                        (list (fterm 'A nil)))
                               (literal nil 'Q
                                        (list (vterm 'w) (fterm 'B nil)))
                               (literal nil 'R
                                        (list (vterm 'w) (vterm 'z))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (vterm 'x) (vterm 'y)))))
                 (clause (list (literal t 'P
                                        (list (fterm 'A nil) (fterm 'B nil)))
                               (literal nil 'Q
                                        (list (fterm 'A nil) (fterm 'B nil))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (fterm 'A nil)))))
                 (clause (list (literal nil 'P
                                        (list (vterm 'x)))
                               (literal t 'Q
                                        (list (vterm 'x))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P nil)))
                 (clause (list (literal t 'P nil))))))

      (is (not (subsumption-clause-p
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'Q (list (vterm 'x) (vterm 'z)))))
              (clause (list (literal nil 'P (list (fterm 'A nil)))
                            (literal nil 'Q (list (vterm 'w) (vterm 'u)))
                            (literal nil 'R (list (vterm 'w))))))))
      )

(test clover.tests.unify.subsumption-clause-p.test3
      (is (subsumption-clause-p
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
              (clause (list (literal nil 'P (list (fterm 'A nil)))
                            (literal nil 'Q (list (vterm 'w) (vterm 'u)))
                            (literal nil 'R (list (vterm 'w)))))))
      )

