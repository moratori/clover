(defpackage clover.tests.rewrite
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.rewrite
        :1am))
(in-package :clover.tests.rewrite)


(test clover.tests.rewrite.rewrite-final.test1

      (let* ((target
              (fterm 'f (list (fterm 'f (list (constant 'A ) (vterm 'y)))
                              (constant 'B ))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'f (list (vterm 'x) (vterm 'y)))
                                (vterm 'x)))))
            (result
              (rewrite-final target rule-set))
            (expected
              (constant 'A )))

        (is (term= result expected)))

      (let* ((target
               (fterm 'reverse (list (fterm 'cons (list (constant 'A )
                                                        (fterm 'cons (list (constant 'B )
                                                                           (constant 'NIL )))))))
              )
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'append (list (constant 'NIL ) (vterm 'x)))
                                (vterm 'x))
                  (rewrite-rule (fterm 'append (list (fterm 'cons
                                                            (list (vterm 'x)
                                                                  (vterm 'y)))
                                                     (vterm 'z)))
                                (fterm 'cons (list (vterm 'x)
                                                   (fterm 'append (list (vterm 'y)
                                                                        (vterm 'z))))))
                  (rewrite-rule (fterm 'reverse (list (constant 'NIL ) ))
                                (constant 'NIL ))

                  (rewrite-rule (fterm 'reverse (list (fterm 'reverse (list
                                                                        (vterm 'x))) ))
                                (vterm 'x))
                  (rewrite-rule (fterm 'reverse (list (fterm 'cons (list (vterm 'x)
                                                                         (vterm 'y))) ))
                                (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                                     (fterm 'cons (list (vterm 'x)
                                                                        (constant 'NIL ))))))
                  (rewrite-rule (fterm 'reverse (list (fterm 'append (list (vterm 'x)
                                                                           (fterm 'cons (list (vterm 'y)
                                                                                              (constant 'NIL )))))))
                                (fterm 'cons (list (vterm 'y)
                                                   (fterm 'reverse (list (vterm 'x))))))
                  )))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'cons (list (constant 'B )
                                 (fterm 'cons (list (constant 'A )
                                                    (constant 'NIL )))))))

        (is (term= result expected)))
      )


(test clover.tests.rewrite.rewrite-final.test2

      (let* ((target
               (fterm 'reverse (list (vterm 'x))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'reverse (list (constant 'NIL ) ))
                                (constant 'NIL ))
                  )))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'reverse (list (vterm 'x)))))

        (is (term= result expected)))
      )

(test clover.tests.rewrite.rewrite-final.test3

      (let* ((target
               (fterm 'reverse (list (constant 'NIL ))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'reverse (list (constant 'NIL ) ))
                                (constant 'NIL ))
                  )))
            (result
              (rewrite-final target rule-set))
            (expected
              (constant 'NIL )))

        (is (term= result expected)))
      )

(test clover.tests.rewrite.rewrite-final.test4

      (let* ((target
               (fterm 'Inv (list
                             (fterm 'Inv (list 
                                           (fterm 'Inv (list 
                                                         (fterm 'Inv (list (fterm 'mult (list (vterm 'x)
                                                                                              (fterm 'Inv (list (vterm 'x))))))))))))))

             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'Inv (list (constant 'E )))
                                (constant 'E ))
                  (rewrite-rule (fterm 'mult (list (constant 'E ) (vterm 'x)))
                                (vterm 'x))
                  (rewrite-rule (fterm 'mult (list (vterm 'x) (constant 'E )))
                                (vterm 'x))
                  (rewrite-rule (fterm 'Inv (list (fterm 'Inv (list (vterm 'x)))))
                                (vterm 'x))
                  (rewrite-rule (fterm 'mult (list (fterm 'Inv (list (vterm 'x)))
                                                   (vterm 'x)
                                                   ))
                                (constant 'E ))
                  (rewrite-rule (fterm 'mult (list (vterm 'x) 
                                                   (fterm 'Inv (list (vterm 'x)))))
                                (constant 'E ))
                  (rewrite-rule (fterm 'Inv (list (fterm 'mult (list (vterm 'x)
                                                                     (vterm 'y)))))
                                (fterm 'mult (list (fterm 'Inv (list (vterm 'y)))
                                                   (fterm 'Inv (list (vterm 'x))))))
                  (rewrite-rule (fterm 'mult (list (fterm 'mult (list (vterm 'x)
                                                                      (vterm 'y)))
                                                   (vterm 'z)))
                                (fterm 'mult (list (vterm 'x)
                                                   (fterm 'mult (list (vterm 'y)
                                                                      (vterm 'z))))))
                  (rewrite-rule (fterm 'mult (list (fterm 'Inv (list (vterm 'x)))
                                                   (fterm 'mult (list (vterm 'x)
                                                                      (vterm 'y)))))
                                (vterm 'y))
                  (rewrite-rule (fterm 'mult (list (vterm 'x)
                                                   (fterm 'mult (list (fterm 'Inv (list (vterm 'x)))
                                                                      (vterm 'y)))))
                                (vterm 'y)))))
            (result
              (rewrite-final target rule-set))
            (expected
              (constant 'E )))

        (is (term= result expected)))
      )


(test clover.tests.rewrite.apply-rewrite-rule.test1
      (let* ((target 
               (fterm 'f (list (vterm 'x))))
             (rule
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (constant 'A)))
             (result
               (apply-rewrite-rule target rule))
             (expected
               (constant 'A)))
        (is (term= result expected))
        )
      )

(test clover.tests.rewrite.apply-rewrite-rule.test2
      (let* ((target 
               (fterm 'mult (list (vterm 'x) (fterm 'mult (list (vterm 'y) (vterm 'z))))))
             (rule
               (rewrite-rule
                 (fterm 'mult (list (fterm 'inv (list (vterm 'w))) (vterm 'w)))
                 (constant 'ONE)))
             (result
               (apply-rewrite-rule target rule))
             (expected
               target))
        (is (term= result expected))
        )
      )

(test clover.tests.rewrite.find-critical-pair.test1
      (let* ((target 
               (fterm 'f (list (vterm 'x))))
             (rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (constant 'A)))
             (rule2
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (fterm 'g (list (vterm 'z)))))
             (expected
               (equation nil (constant 'A) (fterm 'g (list (vterm 'x)))))
             (result
               (find-critical-pair target rule1 rule2)))
        (is (equation= result expected))))

(test clover.tests.rewrite.find-critical-pair.test2
      (let* ((target 
               (fterm 'f (list (vterm 'x))))
             (rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (fterm 'h (list (vterm 'z)))
                 ))
             (rule2
               (rewrite-rule
                 (fterm 'f (list (vterm 'w)))
                 (fterm 'g (list (vterm 'w)))))
             (expected
               (equation nil (fterm 'h (list (vterm 'x)))  (fterm 'g (list (vterm 'x)))))
             (result
               (find-critical-pair target rule1 rule2)))
        (is (equation= result expected))))


(test clover.tests.rewrite.all-critical-pair.test1
      (let* ((rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (constant 'A)))
             (rule2
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (fterm 'g (list (vterm 'z)))))
             (rule-set
               (rewrite-rule-set
                 (list rule1 rule2)))
             (result
               (all-critical-pair rule-set)))
        (is result))
      )
