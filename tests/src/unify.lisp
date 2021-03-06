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


(test clover.tests.unify.alphabet-clause=.test1
      (is (alphabet-clause=
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (vterm 'z)))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (vterm 'v)))))))))
      (is (alphabet-clause=
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (fterm 'A nil)))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (fterm 'A nil)))))))))
      (is (alphabet-clause= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet-clause= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet-clause= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v))))))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'g (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (fterm 'A nil)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (fterm 'A nil)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (fterm 'A nil)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'A nil)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-clause= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'Q (list (vterm 'v)))
                          (literal nil 'P (list (vterm 'w) (vterm 'u))))))))
)


(test clover.tests.unify.alphabet-clause=.test2
      (is (not (alphabet-clause= 
                 (clause (list (literal nil 'P (list (vterm 'x)
                                                     (vterm 'v)
                                                     (vterm 'w)))
                               (literal t   'P (list (vterm 'y)
                                                     (vterm 'z)
                                                     (vterm 'v)))
                               (literal t   'P (list (vterm 'x)
                                                     (vterm 'y)
                                                     (vterm 'u)))
                               (literal t   'P (list (vterm 'u)
                                                     (vterm 'z)
                                                     (vterm 'w)))))
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
                                                     (vterm 'w))))))))
      )


(test clover.tests.unify.alphabet-clause-set=.test1
      (let* ((clause1
              (clause
                (list 
                  (literal nil 'P (list (vterm 'x)
                                        (vterm 'y)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'x))))))))
            (clause2
              (clause
                (list 
                  (literal t 'R (list (vterm 'z))))))
            (clause3
              (clause
                (list 
                  (literal nil 'P (list (vterm 'w)
                                        (vterm 'u)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'w))))))))
            (clause4
              (clause
                (list 
                  (literal t 'R (list (vterm 'v))))))
            (clause-set1
              (clause-set
                (list clause1 clause2)))
            (clause-set2
              (clause-set 
                (list clause4 clause3))))
        (is (alphabet-clause-set= clause-set2 clause-set1))))

(test clover.tests.unify.alphabet-clause-set=.test1
      (let* ((clause1
              (clause
                (list 
                  (literal nil 'P (list (vterm 'x)
                                        (vterm 'y)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'x))))))))
            (clause2
              (clause
                (list 
                  (literal t 'R (list (vterm 'z))))))
            (clause3
              (clause
                (list 
                  (literal nil 'P (list (vterm 'w)
                                        (vterm 'u)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'u))))))))
            (clause4
              (clause
                (list 
                  (literal t 'R (list (vterm 'v))))))
            (clause-set1
              (clause-set
                (list clause1 clause2)))
            (clause-set2
              (clause-set 
                (list clause4 clause3))))
        (is (not (alphabet-clause-set= clause-set2 clause-set1)))))
