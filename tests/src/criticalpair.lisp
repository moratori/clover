(defpackage clover.tests.criticalpair
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.rewrite
        :clover.rename
        :clover.criticalpair
        :1am))
(in-package :clover.tests.criticalpair)


(defun term-set= (list1 list2)
  (and 
    (null (set-difference list1 list2 :test #'term=))
    (null (set-difference list2 list1 :test #'term=))))


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
