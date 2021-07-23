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


(test clover.tests.unify.find-most-general-unifier-set.test1

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
          (ununifiable-error (e) t)))

      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal t 'P (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-error (e) t)))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-error (e) t)))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'P (list (vterm 'x) (vterm 'x)))
              (literal t   'P (list (fterm 'f (list (vterm 'w))) 
                                    (fterm 'g (list (vterm 'v))))))
          (ununifiable-error (e) t)))

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
                                     (list (constant 'A ) (constant 'B )))
                            (literal nil 'Q
                                     (list (constant 'A ) (constant 'B )))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))))
              (clause (list (literal nil 'P 
                                     (list (constant 'A ) 
                                           (fterm 'f (list (vterm 'z)))))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))
                            (literal nil 'Q
                                     (list (vterm 'y) (vterm 'x)))))

              (clause (list (literal nil 'P 
                                     (list (constant 'B ) 
                                           (fterm 'f (list (vterm 'z)
                                                           (fterm 'g (list (vterm 'w)))))))
                            (literal nil 'Q
                                     (list (fterm 'f (list (vterm 'z)
                                                           (fterm 'g (list (vterm 'w)))))
                                           (constant 'B )))
                            (literal t 'R 
                                     (list (constant 'A )
                                           (fterm 'f (list (vterm 'z))))))))))

(test clover.tests.unify.subsumption-clause-p.test2
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (vterm 'x)))
                               (literal nil 'Q
                                        (list (vterm 'x) (vterm 'y)))))
                 (clause (list (literal nil 'P
                                        (list (constant 'A )))
                               (literal nil 'Q
                                        (list (vterm 'w) (constant 'B )))
                               (literal nil 'R
                                        (list (vterm 'w) (vterm 'z))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (vterm 'x) (vterm 'y)))))
                 (clause (list (literal t 'P
                                        (list (constant 'A ) (constant 'B )))
                               (literal nil 'Q
                                        (list (constant 'A ) (constant 'B ))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (constant 'A )))))
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
              (clause (list (literal nil 'P (list (constant 'A )))
                            (literal nil 'Q (list (vterm 'w) (vterm 'u)))
                            (literal nil 'R (list (vterm 'w))))))))
      )

(test clover.tests.unify.subsumption-clause-p.test3
      (is (subsumption-clause-p
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
              (clause (list (literal nil 'P (list (constant 'A )))
                            (literal nil 'Q (list (vterm 'w) (vterm 'u)))
                            (literal nil 'R (list (vterm 'w)))))))
      )


(test clover.tests.unify.alphabet=.test1
      (is (alphabet=
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (vterm 'z)))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (vterm 'v)))))))))
      (is (alphabet=
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (constant 'A )))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (constant 'A )))))))))
      (is (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v))))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'g (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (constant'A )))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'Q (list (vterm 'v)))
                          (literal nil 'P (list (vterm 'w) (vterm 'u))))))))
)


(test clover.tests.unify.alphabet=.test2
      (is (not (alphabet= 
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


(test clover.tests.unify.alphabet=.test3
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
        (is (alphabet= clause-set2 clause-set1))))

(test clover.tests.unify.alphabet=.test4
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
        (is (not (alphabet= clause-set2 clause-set1)))))

(test clover.tests.unify.alphabet=.test5
      (is (not (alphabet= 
                 (fterm 'f (list (vterm 'x) (vterm 'y)))
                 (fterm 'f (list (vterm 'w) (vterm 'w))))))
      (is (alphabet= 
            (fterm 'f (list (vterm 'x) (vterm 'y)))
            (fterm 'f (list (vterm 'u) (vterm 'v)))))
      (is (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'w)))))))
      (is (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y) (fterm 'h (list (vterm 'z) (fterm 'i (list (vterm 'p)))))))))
            (fterm 'f (list (vterm 'u) (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'w) (fterm 'i (list (vterm 'k)))))))))))
      (is (not (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y) (fterm 'h (list (vterm 'z) (fterm 'i (list (vterm 'p)))))))))
            (fterm 'f (list (vterm 'u) (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'w) (vterm 'u))))))))))
      (is (not (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (vterm 'u))))))
      (is (not (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'z))))))))
      (is (not (alphabet= 
            (fterm 'f (list (fterm 'g (list (vterm 'y))) (vterm 'x) ))
            (fterm 'f (list (fterm 'g (list (vterm 'z))) (vterm 'z) )))))
      (is (not (alphabet= 
            (fterm 'f (list (fterm 'g (list (vterm 'y))) (vterm 'x) ))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'z))) )))))
      (is (alphabet= (constant 'A) (constant 'A)))
      (is (alphabet= (vterm 'x) (vterm 'y)))

      (is (not (alphabet= (constant 'B) (constant 'A))))
      (is (alphabet= (vterm 'x) (vterm 'x)))

      (is (not (alphabet= (constant 'B) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet= (vterm 'x) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet= (vterm 'y) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet= (constant 'B) (vterm 'b))))
      (is (alphabet= (constant 'B) (constant 'B)))
      (is (alphabet= (constant 'B) (fterm 'B nil))))

(test clover.tests.unify.alphabet=.test6
      (let ((criteria1
              (rewrite-rule
                (fterm 'f (list (vterm 'x) (vterm 'y)))
                (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
        (is 
          (alphabet= criteria1
                     (rewrite-rule
                       (fterm 'f (list (vterm 'u) (vterm 'v)))
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'z)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'w) (vterm 'w)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'y)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (alphabet=
            (rewrite-rule
              (fterm 'f (list (vterm 'x)))
              (vterm 'x))
            (rewrite-rule
              (fterm 'f (list (vterm 'z)))
              (vterm 'z))))
        (is 
          (not (alphabet=
            (rewrite-rule
              (fterm 'f (list (vterm 'x)))
              (vterm 'x))
            (rewrite-rule
              (fterm 'f (list (vterm 'y)))
              (vterm 'z)))))
        (is 
          (alphabet=
            (rewrite-rule
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (rewrite-rule
              (vterm 'z)
              (fterm 'f (list (vterm 'z))))))
        (is 
          (not (alphabet=
            (rewrite-rule
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (rewrite-rule
              (vterm 'z)
              (fterm 'f (list (vterm 'w)))))))
        (is 
          (alphabet=
            (rewrite-rule
              (vterm 'x)
              (vterm 'x))
            (rewrite-rule
              (vterm 'z)
              (vterm 'z))))
        (is 
          (alphabet=
            (rewrite-rule
              (vterm 'y)
              (vterm 'z))
            (rewrite-rule
              (vterm 'w)
              (vterm 't))))
        (is 
          (not (alphabet=
            (rewrite-rule
              (vterm 'x)
              (vterm 'x))
            (rewrite-rule
              (vterm 'y)
              (vterm 'z)))))))

(test clover.tests.unify.alphabet=.test7
      (let ((criteria1
              (equation nil
                (fterm 'f (list (vterm 'x) (vterm 'y)))
                (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
        (is 
          (alphabet= criteria1
                     (equation nil
                       (fterm 'f (list (vterm 'u) (vterm 'v)))
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v))))))))
        (is 
          (alphabet= criteria1
                     (equation nil
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v)))))
                       (fterm 'f (list (vterm 'u) (vterm 'v))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'z)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'w) (vterm 'w)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'y)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not (alphabet=
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation t
              (vterm 'z)
              (fterm 'f (list (vterm 'z)))))))
        (is 
          (alphabet=
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation nil
              (vterm 'z)
              (fterm 'f (list (vterm 'z))))))
        (is 
          (not (alphabet=
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation nil
              (vterm 'z)
              (fterm 'f (list (vterm 'w)))))))
        (is 
          (alphabet=
            (equation nil
              (vterm 'x)
              (vterm 'x))
            (equation nil
              (vterm 'z)
              (vterm 'z))))
        (is 
          (alphabet=
            (equation nil
              (vterm 'y)
              (vterm 'z))
            (equation nil
              (vterm 'w)
              (vterm 't))))))

(test clover.tests.unify.alphabet=.test8
      (is 
          (not (alphabet=
            (equation nil
              (fterm 'plus (list (vterm 'x) (vterm 'y)))
              (fterm 'plus (list (fterm 'plus (list (vterm 'x) 
                                                    (constant 'ZERO)))
                                 (vterm 'y))))
            (equation nil
              (fterm 'plus (list (fterm 'plus (list (fterm 'inv (list (vterm 'u)))
                                                    (constant 'ZERO)))
                                 (vterm 'u)))
              (constant 'ZERO)
              ))))
      )

(test clover.tests.unify.find-most-general-unifier-set.test2

      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
                  (fterm 'plus (list (vterm 'u) (fterm 'inv (list (vterm 'u))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'inv (list (constant 'ZERO))))
                    (unifier (vterm 'u) (constant 'ZERO)))))))
      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (fterm 'plus (list (vterm 'u) (fterm 'inv (list (vterm 'u)))))
                  (fterm 'plus (list (constant 'ZERO) (vterm 'x))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'inv (list (constant 'ZERO))))
                    (unifier (vterm 'u) (constant 'ZERO)))))))
      )

