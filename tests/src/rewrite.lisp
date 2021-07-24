(defpackage clover.tests.rewrite
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.rewrite
        :clover.rename
        :1am))
(in-package :clover.tests.rewrite)


(defun term-set= (list1 list2)
  (and 
    (null (set-difference list1 list2 :test #'term=))
    (null (set-difference list2 list1 :test #'term=))))


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

      (let* ((target
               (fterm 'f (list (vterm 'x) (vterm 'y))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'f (list (vterm 'u) (vterm 'u)))
                                (vterm 'u)))))
            (result
              (rewrite-final target rule-set))
            (expected
              target))

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


(test clover.tests.rewrite.rewrite-final.test5
      (let* ((target 
               (fterm 'f (list (vterm 'x))))
             (rule
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (constant 'A)))
             (result
               (rewrite-final target rule))
             (expected
               (constant 'A)))
        (is (term= result expected))
        )
      )

(test clover.tests.rewrite.rewrite-final.test6
      (let* ((target 
               (fterm 'mult (list (vterm 'x) (fterm 'mult (list (vterm 'y) (vterm 'z))))))
             (rule
               (rewrite-rule
                 (fterm 'mult (list (fterm 'inv (list (vterm 'w))) (vterm 'w)))
                 (constant 'ONE)))
             (result
               (rewrite-final target rule))
             (expected
               target))
        (is (term= result expected))
        )
      )

(test clover.tests.rewrite.rewrite-all-ways.test1
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
             (rule-set
               (rewrite-rule-set
                 (list rule1 rule2)))
             (expected
               (list (constant 'A)
                     (fterm 'g (list (vterm 'x)))))
             (result
               (rewrite-all-ways target rule-set)))
        (is (term-set= result expected))))


(test clover.tests.rewrite.rewrite-all-ways.test2
      (let* ((target 
               (fterm 'f (list (vterm 'x))))
             (rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (fterm 'h (list (vterm 'z)))))
             (rule2
               (rewrite-rule
                 (fterm 'f (list (vterm 'w)))
                 (fterm 'g (list (vterm 'w)))))
             (rule-set
               (rewrite-rule-set
                 (list rule1 rule2)))
             (expected
               (list
                 (fterm 'h (list (vterm 'x)))
                 (fterm 'g (list (vterm 'x)))))
             (result
               (rewrite-all-ways target rule-set)))
        (is (term-set= result expected))))


(test clover.tests.rewrite.rewrite-all-ways.test3
      (let* ((target 
               (fterm 'plus (list (fterm 'plus (list (constant 'ZERO) (vterm 'x))) (vterm 'y))))
             (rule1
               (rewrite-rule
                 (fterm 'plus (list (constant 'ZERO) (vterm 'w)))
                 (vterm 'w)))
             (rule2
               (rewrite-rule
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                 (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))))
             (rule-set
               (rewrite-rule-set
                 (list rule1 rule2)))
             (expected
               (list
                 (fterm 'plus (list (vterm 'x) (vterm 'y)))
                 (fterm 'plus (list (constant 'ZERO) (fterm 'plus (list (vterm 'x) (vterm 'y)))))))
             (result
               (rewrite-all-ways target rule-set)))
        (is (term-set= result expected))))


(test clover.tests.rewrite.rewrite-all-ways.test4
      (let* ((target 
               (fterm 'plus (list 
                              (fterm 'plus 
                                     (list (fterm 'inv (list (vterm 'x))) (vterm 'x))) 
                              (vterm 'y))))
             (rule1
               (rewrite-rule
                 (fterm 'plus (list (fterm 'inv (list (vterm 'w))) (vterm 'w)))
                 (constant 'ZERO)))
             (rule2
               (rewrite-rule
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                 (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))))
             (rule-set
               (rewrite-rule-set 
                 (list rule1 rule2)))
             (expected
               (list 
                 (fterm 'plus (list (constant 'ZERO) (vterm 'y)))
                 (fterm 'plus (list (fterm 'inv (list (vterm 'x))) 
                                    (fterm 'plus (list (vterm 'x) (vterm 'y)))))))
             (result
               (rewrite-all-ways target rule-set)))
        (is (term-set= result expected))))

(test clover.tests.rewrite.rewrite-all-ways.test5
      (let* ((target 
               (fterm 'plus (list 
                              (fterm 'plus 
                                     (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                           (vterm 'z))) 
                              (vterm 'w))))
             (rule1
               (rewrite-rule
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                 (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))))
             (rule2
               (rewrite-rule
                 (fterm 'plus (list (fterm 'plus (list (vterm 'v) (vterm 'w))) (vterm 'u)))
                 (fterm 'plus (list (vterm 'v) (fterm 'plus (list (vterm 'w) (vterm 'u)))))))
             (rules-set
               (rewrite-rule-set (list rule1 rule2)))
             (expected
               (list 
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z))))) 
                                    (vterm 'w)))
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y)))
                                    (fterm 'plus (list (vterm 'z) (vterm 'w)))))))
             (result
               (rewrite-all-ways target rules-set)))
        (is (term-set= result expected))))


(test clover.tests.rewrite.find-subterms.test1
      (let* ((a 
               (fterm 'plus (list (constant 'ZERO)
                                  (vterm 'x))))
             (b 
               (fterm 'plus (list (fterm 'plus (list (vterm 'w) (vterm 'u)))
                                  (vterm 'v))))
             (result
               (find-subterms a b))
             (expected
               (fterm 'plus (list (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
                                  (vterm 'v)))))
        (is (member expected result :test #'term=))
        )
      )

(test clover.tests.rewrite.find-subterms.test2
      (let* ((a 
               (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                  (vterm 'x))))
             (b 
               (fterm 'plus (list (fterm 'plus (list (vterm 'w) (vterm 'u)))
                                  (vterm 'v))))
             (result
               (find-subterms a b))
             (expected
               (fterm 'plus (list (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'x)))
                                  (vterm 'v)))))
        (is (member expected result :test #'term=))
        )
      )

(test clover.tests.rewrite.find-subterms.test3
      (let* ((a 
               (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y)))
                                  (vterm 'z))))
             (b 
               (fterm 'plus (list (fterm 'plus (list (vterm 'w) (vterm 'u)))
                                  (vterm 'v))))
             (result
               (find-subterms a b))
             (expected
               (fterm 'plus (list (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y)))
                                                     (vterm 'z)))
                                  (vterm 'v)))
               ))
        (is (member expected result :test #'term=))))

(test clover.tests.rewrite.find-subterms.test4
      (let* ((a 
               (fterm 'plus (list (vterm 'x))))
             (b 
               (fterm 'plus (list (vterm 'y))))
             (result
               (find-subterms a b))
             (expected
               a))
        (is (member expected result :test #'term=))))

(test clover.tests.rewrite.find-subterms.test5
      (let* ((a 
               (fterm 'plus (list (constant 'ZERO) (vterm 'x))))
             (b 
               (fterm 'plus (list (vterm 'u) (fterm 'inv (list (vterm 'u))))))
             (result
               (find-subterms a b))
             (expected
               (fterm 'plus (list (constant 'ZERO)
                                  (fterm 'inv (list (constant 'ZERO)))))))
        (is (member expected result :test #'term=))))

(test clover.tests.rewrite.find-subterms.test6
      (let* ((a 
               (fterm 'f (list (fterm 'inv (list (vterm 'u))) 
                               (fterm 'g (list (vterm 'w))) 
                               (vterm 'z)))
               )
             (b 
               (fterm 'f (list (vterm 'x)
                               (fterm 'g (list (vterm 'x))) 
                               (fterm 'h (list (vterm 'x) (vterm 'y))))))
             (result
               (find-subterms a b))
             (expected
               (list (fterm 'f (list (fterm 'inv (list (vterm 'u)))
                               (fterm 'g (list (fterm 'inv (list (vterm 'u)))))
                               (fterm 'h (list (fterm 'inv (list (vterm 'u)))
                                               (vterm 'y))))))))
        (is (term-set= result expected))))


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
               (rename-for-human-readable-printing (all-critical-pair rule-set)))
             (expected
               (equation-set
                 (list (equation nil 
                                 (constant 'A)
                                 (fterm 'g (list (vterm 'CLOVER.PARSER::X))))))))
        (is 
          (equation-set= expected result)))

      (let* ((rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'x) (vterm 'x)))
                 (vterm 'x)
                 ))
             (rule2
               (rewrite-rule
                 (fterm 'g (list (fterm 'f (list (vterm 'u) (vterm 'v))) (vterm 'u)))
                 (fterm 'h (list (vterm 'u)))))
             (rule-set
               (rewrite-rule-set
                 (list rule1 rule2))) 
             (result
               (rename-for-human-readable-printing (all-critical-pair rule-set)))
             (expected
               (equation-set
                 (list 
                   (equation
                     nil
                     (fterm 'g (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::X)))
                     (fterm 'h (list (vterm 'CLOVER.PARSER::X))))))))
        (is 
          (equation-set= expected result)))

      (let* ((rule1
               (rewrite-rule
                 (fterm 'op (list (fterm 'op (list (vterm 'x) (vterm 'y)))
                                  (vterm 'z)))
                 (fterm 'op (list (vterm 'x)
                                  (fterm 'op (list (vterm 'y) (vterm 'z)))))))
             (rule2
               (rewrite-rule
                 (fterm 'op (list (constant 'ONE) (vterm 'x)))
                 (vterm 'x)))
             (rule3
               (rewrite-rule
                 (fterm 'op (list (fterm 'inv (list (vterm 'x)))
                                  (vterm 'x)))
                 (constant 'ONE)))
             (rule-set
               (rewrite-rule-set
                 (list rule1 rule2 rule3))) 
             (result
               (rename-for-human-readable-printing
                 (all-critical-pair rule-set)))
             (expected
               (equation-set
                 (list 
                   (equation 
                     nil
                     (fterm 'op (list (vterm 'CLOVER.PARSER::X)
                                      (vterm 'CLOVER.PARSER::Y)))
                     (fterm 'op (list (constant 'ONE)
                                      (fterm 'op (list (vterm 'CLOVER.PARSER::X)
                                                       (vterm 'CLOVER.PARSER::Y))))))
                   (equation 
                     nil
                     (fterm 'op (list (fterm 'inv (list (vterm 'CLOVER.PARSER::X)))
                                      (fterm 'op (list (vterm 'CLOVER.PARSER::X)
                                                       (vterm 'CLOVER.PARSER::Y)))))
                     (fterm 'op (list (constant 'ONE) (vterm 'CLOVER.PARSER::Y))))
                   (equation 
                     nil
                     (fterm 'op (list (fterm 'op (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::Y)))
                                      (fterm 'op (list (vterm 'CLOVER.PARSER::Z) (vterm 'CLOVER.PARSER::W)))))
                     (fterm 'op (list 
                                  (fterm 'op (list (vterm 'CLOVER.PARSER::X)
                                                   (fterm 'op (list (vterm 'CLOVER.PARSER::Y)
                                                                    (vterm 'CLOVER.PARSER::Z)))))
                                  (vterm 'CLOVER.PARSER::W))))))))
        (is 
          (equation-set= result expected)))
      )


(test clover.tests.rewrite-rule.all-critical-pair.test2
      (let* ((target
               (rewrite-rule-set
                 (list 
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                        (vterm 'x)))
                     (constant 'ZERO)
                     )
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                        (vterm 'z)))
                     (fterm 'plus (list (vterm 'x)
                                        (fterm 'plus (list (vterm 'y) (vterm 'z)))))))))
             (result
               (rename-for-human-readable-printing (all-critical-pair target)))
             (expected
               (equation-set
                 (list 
                   (equation 
                     nil
                     (fterm 'plus (list (constant 'ZERO) (vterm 'CLOVER.PARSER::Y)))
                     (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::X)))
                                        (fterm 'plus (list (vterm 'CLOVER.PARSER::X)
                                                           (vterm 'CLOVER.PARSER::Y)))))
                     )
                   (equation
                     nil
                     (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::X)
                                                           (fterm 'plus (list (vterm 'CLOVER.PARSER::Y)
                                                                              (vterm 'CLOVER.PARSER::Z)))))
                                        (vterm 'CLOVER.PARSER::W)))
                     (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::Y)))
                                        (fterm 'plus (list (vterm 'CLOVER.PARSER::Z) (vterm 'CLOVER.PARSER::W)))))
                     )))))

        (is (equation-set= result expected))))

(test clover.tests.rewrite-rule.all-critical-pair.test3
      (let* ((target
               (rewrite-rule-set
                 (list 
                   (rewrite-rule
                     (fterm 'plus (list (constant 'ZERO)
                                        (vterm 'x)))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                        (vterm 'z)))
                     (fterm 'plus (list (vterm 'x)
                                        (fterm 'plus (list (vterm 'y) (vterm 'z)))))))))
             (expected
               (equation-set
                 (list 
                   (equation 
                     nil
                     (fterm 'plus (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::Y)))
                     (fterm 'plus (list (constant 'ZERO)
                                        (fterm 'plus (list (vterm 'CLOVER.PARSER::X)
                                                           (vterm 'CLOVER.PARSER::Y)))))
                     )
                   (equation
                     nil
                     (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::X)
                                                           (fterm 'plus (list (vterm 'CLOVER.PARSER::Y)
                                                                              (vterm 'CLOVER.PARSER::Z)))))
                                        (vterm 'CLOVER.PARSER::W)))
                     (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::Y)))
                                        (fterm 'plus (list (vterm 'CLOVER.PARSER::Z) (vterm 'CLOVER.PARSER::W)))))
                     )))
               )
             (result
               (rename-for-human-readable-printing (all-critical-pair target))))

        (is (equation-set= result expected) )))


(test clover.tests.rewrite-rule.all-critical-pair.test4
      (let ((target
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'plus (list (constant 'ZERO)
                                       (vterm 'x)))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x)))
                    (constant 'ZERO))))))
        (is 
          (null (equation-set.equations (all-critical-pair target))))))

(test clover.tests.rewrite-rule.all-critical-pair.test5
      (let* ((target
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (fterm 'plus (list (vterm 'x) (vterm 'y)))))
                    (vterm 'y))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (fterm 'plus (list (vterm 'x) (vterm 'y)))))
                    (vterm 'y)))))
            (expected
              (equation-set
                (list 
                  (equation
                    nil
                    (fterm 'plus (list (fterm 'inv (list (fterm 'inv (list (vterm 'CLOVER.PARSER::X))))) (vterm 'CLOVER.PARSER::Y)))
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::Y)))))))
            (result
              (rename-for-human-readable-printing (all-critical-pair target)))
            )
        (is (equation-set= expected result))))

(test clover.tests.rewrite-rule.all-critical-pair.test8
      (let* ((target
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'inv (list (vterm 'x)))))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x)))
                    (constant 'ZERO)))))
             (expected
               (equation-set
                 (list 
                   (equation
                     nil
                     (constant 'ZERO)
                     (fterm 'plus (list (vterm 'CLOVER.PARSER::X) 
                                        (fterm 'inv (list (vterm 'CLOVER.PARSER::X)))))))))
            (result
              (rename-for-human-readable-printing (all-critical-pair target))))
        (is (equation-set= result expected))))

(test clover.tests.rewrite-rule.all-critical-pair.test9
      (let* ((target
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'plus (list (constant 'ZERO) 
                                       (vterm 'x)))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'x)
                                       (fterm 'inv (list (vterm 'x)))))
                    (constant 'ZERO)))))
             (expected
               (equation-set
                 (list 
                   (equation
                     nil
                     (constant 'ZERO)
                     (fterm 'inv (list (constant 'ZERO)))))))
            (result
              (rename-for-human-readable-printing (all-critical-pair target))))
        (is (equation-set= result expected))))
