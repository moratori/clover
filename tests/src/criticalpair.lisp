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


(test clover.tests.criticalpair.critical-pair.test1
      (let* ((rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'z)))
                 (constant 'A)))
             (rule2
               (rewrite-rule
                 (fterm 'f (list (vterm 'x)))
                 (fterm 'g (list (vterm 'x)))))
             (result
               (rename-for-human-readable-printing (critical-pair rule1 rule2)))
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
             (result1
               (rename-for-human-readable-printing (critical-pair rule1 rule2)))
             (result2
               (rename-for-human-readable-printing (critical-pair rule2 rule1)))
             (expected
               (equation-set
                 (list 
                   (equation
                     nil
                     (fterm 'g (list (vterm 'CLOVER.PARSER::X) (vterm 'CLOVER.PARSER::X)))
                     (fterm 'h (list (vterm 'CLOVER.PARSER::X))))))))
        (is (equation-set= expected result1))
        (is (not (equation-set= expected result2))))

      )


(test clover.tests.criticalpair.all-critical-pair.test1
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


(test clover.tests.criticalpair.all-critical-pair.test2
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

(test clover.tests.criticalpair.all-critical-pair.test3
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


(test clover.tests.criticalpair.all-critical-pair.test4
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

(test clover.tests.criticalpair.all-critical-pair.test5
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

(test clover.tests.criticalpair.all-critical-pair.test8
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

(test clover.tests.criticalpair.all-critical-pair.test9
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
