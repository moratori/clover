(defpackage clover.tests.parser
  (:use :cl
        :clover.property
        :clover.types
        :clover.parser
        :clover.util
        :1am))
(in-package :clover.tests.parser)


(test clover.tests.parser.parse-premise-logical-expression.test1
      
      (is (clause=
               (parse-premise-logical-expression "pred(x)")
               (clause 
                 (list 
                   (literal nil 
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::X))))))) 
      
      (is (clause=
               (parse-premise-logical-expression "!pred(x)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::X)))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(x,y)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(f(x),y)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                                  (vterm 'CLOVER.PARSER::Y)))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(y,f(x))")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(g(y),f(x))")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y)))
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(A,f(x))")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::A nil)
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(f(x),A)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                                  (fterm 'CLOVER.PARSER::A nil)))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(g(f(x)),A)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (fterm 'CLOVER.PARSER::A nil)))))))

      (is (clause=
               (parse-premise-logical-expression "!pred(g(f(x)),A) | q(A, B) | !r(x,y,fun(ABC))")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (fterm 'CLOVER.PARSER::A nil)))
                   (literal nil
                            'CLOVER.PARSER::Q
                            (list (fterm 'CLOVER.PARSER::A nil)
                                  (fterm 'CLOVER.PARSER::B nil)))
                   (literal t
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (fterm 'CLOVER.PARSER::ABC nil)))))))))
      
      (is (clause=
               (parse-premise-logical-expression 
                 (format nil "~A" (parse-premise-logical-expression "!pred(g(f(x)),A) | q(A, B) | !r(x,y,fun(ABC))")))
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (fterm 'CLOVER.PARSER::A nil)))
                   (literal nil
                            'CLOVER.PARSER::Q
                            (list (fterm 'CLOVER.PARSER::A nil)
                                  (fterm 'CLOVER.PARSER::B nil)))
                   (literal t
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (fterm 'CLOVER.PARSER::ABC nil)))))))))
      )



(test clover.tests.parser.parse-conseq-logical-expression.test1
      
      (is (clause=
               (parse-conseq-logical-expression "pred(x)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::X))))))) 
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(x)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::X)))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(x,y)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(f(x),y)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                                  (vterm 'CLOVER.PARSER::Y)))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(y,f(x))")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(g(y),f(x))")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y)))
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(A,f(x))")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::A nil)
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(f(x),A)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                                  (fterm 'CLOVER.PARSER::A nil)))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(g(f(x)),A)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (fterm 'CLOVER.PARSER::A nil)))))))

      (is (clause=
               (parse-conseq-logical-expression "!pred(g(f(x)),A) & q(A, B) & !r(x,y,fun(ABC))")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (fterm 'CLOVER.PARSER::A nil)))
                   (literal t
                            'CLOVER.PARSER::Q
                            (list (fterm 'CLOVER.PARSER::A nil)
                                  (fterm 'CLOVER.PARSER::B nil)))
                   (literal nil
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (fterm 'CLOVER.PARSER::ABC nil)))))))))
      
      (is (clause=
               (parse-premise-logical-expression
                 (format nil "~A"(parse-conseq-logical-expression "!pred(g(f(x)),A) & q(A, B) & !r(x,y,fun(ABC))")))
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (fterm 'CLOVER.PARSER::A nil)))
                   (literal t
                            'CLOVER.PARSER::Q
                            (list (fterm 'CLOVER.PARSER::A nil)
                                  (fterm 'CLOVER.PARSER::B nil)))
                   (literal nil
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (fterm 'CLOVER.PARSER::ABC nil)))))))))

      )


(test clover.tests.parser.parse-premise-logical-expression.test2

      (is 
        (clause=
          (parse-premise-logical-expression "A = A")
          (clause (list (equation nil
                   (fterm 'CLOVER.PARSER::A nil)
                   (fterm 'CLOVER.PARSER::A nil))))))

      (is 
        (clause=
          (parse-premise-logical-expression "A = x")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list (fterm 'CLOVER.PARSER::A nil)
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "x = A")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (fterm 'CLOVER.PARSER::A nil)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "x = y")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (vterm 'CLOVER.PARSER::Y)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "f(x) = g(y)")
          (clause (list (equation nil
                    (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                    (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y))))))))

      (is 
        (clause=
          (parse-premise-logical-expression "f(x) = z")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                     (vterm 'CLOVER.PARSER::Z)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "z = f(x)")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::Z)
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))

      (is 
        (clause=
          (parse-premise-logical-expression "s(ZERO) = ONE")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))
                     (fterm 'CLOVER.PARSER::ONE nil)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "ONE = s(ZERO)")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::ONE nil)
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))))))))

      )



(test clover.tests.parser.parse-premise-logical-expression.test3

      (is 
        (clause=
          (parse-premise-logical-expression "A != A")
          (clause (list (equation t
                   (fterm 'CLOVER.PARSER::A nil)
                   (fterm 'CLOVER.PARSER::A nil))))))

      (is 
        (clause=
          (parse-premise-logical-expression "A != x")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list (fterm 'CLOVER.PARSER::A nil)
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "x != A")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (fterm 'CLOVER.PARSER::A nil)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "x != y")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (vterm 'CLOVER.PARSER::Y)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "f(x) != g(y)")
          (clause (list (equation t
                    (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                    (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y))))))))

      (is 
        (clause=
          (parse-premise-logical-expression "f(x) != z")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                     (vterm 'CLOVER.PARSER::Z)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "z != f(x)")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::Z)
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))

      (is 
        (clause=
          (parse-premise-logical-expression "s(ZERO) != ONE")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))
                     (fterm 'CLOVER.PARSER::ONE nil)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "ONE != s(ZERO)")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::ONE nil)
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))))))))

      )


(test clover.tests.parser.parse-conseq-logical-expression.test2

      (is 
        (clause=
          (parse-conseq-logical-expression "A != A")
          (clause (list (equation nil
                   (fterm 'CLOVER.PARSER::A nil)
                   (fterm 'CLOVER.PARSER::A nil))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A != x")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list (fterm 'CLOVER.PARSER::A nil)
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "x != A")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (fterm 'CLOVER.PARSER::A nil)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "x != y")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (vterm 'CLOVER.PARSER::Y)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "f(x) != g(y)")
          (clause (list (equation nil
                    (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                    (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y))))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "f(x) != z")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                     (vterm 'CLOVER.PARSER::Z)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "z != f(x)")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::Z)
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "s(ZERO) != ONE")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))
                     (fterm 'CLOVER.PARSER::ONE nil)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "ONE != s(ZERO)")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::ONE nil)
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))))))))

      )


(test clover.tests.parser.parse-conseq-logical-expression.test3

      (is 
        (clause=
          (parse-conseq-logical-expression "A = A")
          (clause (list (equation t
                   (fterm 'CLOVER.PARSER::A nil)
                   (fterm 'CLOVER.PARSER::A nil))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A = x")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list (fterm 'CLOVER.PARSER::A nil)
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "x = A")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (fterm 'CLOVER.PARSER::A nil)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "x = y")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (vterm 'CLOVER.PARSER::Y)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "f(x) = g(y)")
          (clause (list (equation t
                    (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                    (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y))))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "f(x) = z")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                     (vterm 'CLOVER.PARSER::Z)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "z = f(x)")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::Z)
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "s(ZERO) = ONE")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))
                     (fterm 'CLOVER.PARSER::ONE nil)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "ONE = s(ZERO)")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::ONE nil)
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))))))))

      )


(test clover.tests.parser.parse-conseq-logical-expression.test3

      (is 
        (clause=
          (parse-conseq-logical-expression "A = A & f(x)=g(x)")
          (clause (list 
                    (equation t
                              (fterm 'CLOVER.PARSER::A nil)
                              (fterm 'CLOVER.PARSER::A nil))
                    (equation t
                              (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::X))))
                    ))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A != x & B = B & !pred(x,y)")
          (clause (list (literal nil
                                 'CLOVER.PARSER::=
                                 (list (fterm 'CLOVER.PARSER::A nil)
                                       (vterm 'CLOVER.PARSER::X)))
                        (literal t
                                 'CLOVER.PARSER::=
                                 (list (fterm 'CLOVER.PARSER::B nil)
                                       (fterm 'CLOVER.PARSER::B nil)))
                        (literal nil
                             'CLOVER.PARSER::PRED
                             (list 
                               (vterm 'CLOVER.PARSER::X)
                               (vterm 'CLOVER.PARSER::Y)))
                        ))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A != x & B = B & f(x) != g(x)")
          (clause (list (literal nil
                                 'CLOVER.PARSER::=
                                 (list (fterm 'CLOVER.PARSER::A nil)
                                       (vterm 'CLOVER.PARSER::X)))
                        (literal t
                                 'CLOVER.PARSER::=
                                 (list (fterm 'CLOVER.PARSER::B nil)
                                       (fterm 'CLOVER.PARSER::B nil)))
                        (equation nil
                              (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::X))))
                        ))))

      )

(test clover.tests.parser.parse-premise-logical-expression.test4

      (is 
        (clause=
          (parse-premise-logical-expression "z != f(x) | A = B | f(x) = h(x, y)")
          (clause (list 
                    (literal t
                             'CLOVER.PARSER::=
                             (list 
                               (vterm 'CLOVER.PARSER::Z)
                               (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                    (literal nil
                             'CLOVER.PARSER::=
                             (list 
                               (fterm 'CLOVER.PARSER::A nil)
                               (fterm 'CLOVER.PARSER::B nil)))
                    (literal nil
                             'CLOVER.PARSER::=
                             (list 
                               (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                               (fterm 'CLOVER.PARSER::H (list (vterm 'CLOVER.PARSER::X)
                                                              (vterm 'CLOVER.PARSER::Y)))))))))

      (is 
        (clause=
          (parse-premise-logical-expression "s(ZERO) != ONE | f(x) = A | pred(x,y)")
          (clause (list 
                    (literal t
                             'CLOVER.PARSER::=
                             (list 
                               (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))
                               (fterm 'CLOVER.PARSER::ONE nil)))
                    (equation nil
                              (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::A nil))
                    (literal nil
                             'CLOVER.PARSER::PRED
                             (list 
                               (vterm 'CLOVER.PARSER::X)
                               (vterm 'CLOVER.PARSER::Y)))))))
      )
