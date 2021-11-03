(defpackage clover.tests.parser
  (:use :cl
        :clover.property
        :clover.types
        :clover.parser
        :clover.util
        :clover.conditions
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
                            (list (constant 'CLOVER.PARSER::A)
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(f(x),A)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                                  (constant 'CLOVER.PARSER::A )))))))
      
      (is (clause=
               (parse-premise-logical-expression "!pred(g(f(x)),A)")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (constant 'CLOVER.PARSER::A )))))))

      (is (clause=
               (parse-premise-logical-expression "!pred(g(f(x)),A) | q(A, B) | !r(x,y,fun(ABC))")
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (constant 'CLOVER.PARSER::A )))
                   (literal nil
                            'CLOVER.PARSER::Q
                            (list (constant 'CLOVER.PARSER::A )
                                  (constant 'CLOVER.PARSER::B )))
                   (literal t
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (constant 'CLOVER.PARSER::ABC )))))))))
      
      (is (clause=
               (parse-premise-logical-expression 
                 (format nil "~A" (parse-premise-logical-expression "!pred(g(f(x)),A) | q(A, B) | !r(x,y,fun(ABC))")))
               (clause 
                 (list 
                   (literal t
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (constant 'CLOVER.PARSER::A )))
                   (literal nil
                            'CLOVER.PARSER::Q
                            (list (constant 'CLOVER.PARSER::A )
                                  (constant 'CLOVER.PARSER::B )))
                   (literal t
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (constant 'CLOVER.PARSER::ABC )))))))))
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
                            (list (constant 'CLOVER.PARSER::A )
                                  (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(f(x),A)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                                  (constant 'CLOVER.PARSER::A )))))))
      
      (is (clause=
               (parse-conseq-logical-expression "!pred(g(f(x)),A)")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (constant 'CLOVER.PARSER::A )))))))

      (is (clause=
               (parse-conseq-logical-expression "!pred(g(f(x)),A) & q(A, B) & !r(x,y,fun(ABC))")
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (constant 'CLOVER.PARSER::A )))
                   (literal t
                            'CLOVER.PARSER::Q
                            (list (constant 'CLOVER.PARSER::A )
                                  (constant 'CLOVER.PARSER::B )))
                   (literal nil
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (constant 'CLOVER.PARSER::ABC )))))))))
      
      (is (clause=
               (parse-premise-logical-expression
                 (format nil "~A"(parse-conseq-logical-expression "!pred(g(f(x)),A) & q(A, B) & !r(x,y,fun(ABC))")))
               (clause 
                 (list 
                   (literal nil
                            'CLOVER.PARSER::PRED
                            (list (fterm 'CLOVER.PARSER::G (list (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))
                                  (constant 'CLOVER.PARSER::A )))
                   (literal t
                            'CLOVER.PARSER::Q
                            (list (constant 'CLOVER.PARSER::A )
                                  (constant 'CLOVER.PARSER::B )))
                   (literal nil
                            'CLOVER.PARSER::R
                            (list (vterm 'CLOVER.PARSER::X)
                                  (vterm 'CLOVER.PARSER::Y)
                                  (fterm 'CLOVER.PARSER::FUN (list (constant 'CLOVER.PARSER::ABC )))))))))

      )


(test clover.tests.parser.parse-premise-logical-expression.test2

      (is 
        (clause=
          (parse-premise-logical-expression "A = A")
          (clause (list (equation nil
                   (constant 'CLOVER.PARSER::A )
                   (constant 'CLOVER.PARSER::A ))))))

      (is 
        (clause=
          (parse-premise-logical-expression "A = x")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list (constant 'CLOVER.PARSER::A )
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "x = A")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (constant 'CLOVER.PARSER::A )))))))

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
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))
                     (constant 'CLOVER.PARSER::ONE)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "ONE = s(ZERO)")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (constant 'CLOVER.PARSER::ONE )
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))))))))

      )



(test clover.tests.parser.parse-premise-logical-expression.test3

      (is 
        (clause=
          (parse-premise-logical-expression "A != A")
          (clause (list (equation t
                   (constant 'CLOVER.PARSER::A )
                   (constant 'CLOVER.PARSER::A ))))))

      (is 
        (clause=
          (parse-premise-logical-expression "A != x")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list (constant 'CLOVER.PARSER::A )
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "x != A")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (constant 'CLOVER.PARSER::A )))))))

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
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))
                     (constant 'CLOVER.PARSER::ONE )))))))

      (is 
        (clause=
          (parse-premise-logical-expression "ONE != s(ZERO)")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (constant 'CLOVER.PARSER::ONE )
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))))))))

      )


(test clover.tests.parser.parse-conseq-logical-expression.test2

      (is 
        (clause=
          (parse-conseq-logical-expression "A != A")
          (clause (list (equation nil
                   (constant 'CLOVER.PARSER::A )
                   (constant 'CLOVER.PARSER::A ))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A != x")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list (constant 'CLOVER.PARSER::A )
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "x != A")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (constant 'CLOVER.PARSER::A )))))))

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
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))
                     (constant 'CLOVER.PARSER::ONE )))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "ONE != s(ZERO)")
          (clause (list (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (constant 'CLOVER.PARSER::ONE )
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))))))))

      )


(test clover.tests.parser.parse-conseq-logical-expression.test3

      (is 
        (clause=
          (parse-conseq-logical-expression "A = A")
          (clause (list (equation t
                   (constant 'CLOVER.PARSER::A )
                   (constant 'CLOVER.PARSER::A ))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A = x")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list (constant 'CLOVER.PARSER::A )
                         (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "x = A")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (constant 'CLOVER.PARSER::A )))))))

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
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))
                     (constant 'CLOVER.PARSER::ONE )))))))

      (is 
        (clause=
          (parse-conseq-logical-expression "ONE = s(ZERO)")
          (clause (list (literal t
                   'CLOVER.PARSER::=
                   (list 
                     (constant 'CLOVER.PARSER::ONE )
                     (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))))))))

      )


(test clover.tests.parser.parse-conseq-logical-expression.test3

      (is 
        (clause=
          (parse-conseq-logical-expression "A = A & f(x)=g(x)")
          (clause (list 
                    (equation t
                              (constant 'CLOVER.PARSER::A )
                              (constant 'CLOVER.PARSER::A ))
                    (equation t
                              (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::X))))
                    ))))

      (is 
        (clause=
          (parse-conseq-logical-expression "A != x & B = B & !pred(x,y)")
          (clause (list (literal nil
                                 'CLOVER.PARSER::=
                                 (list (constant 'CLOVER.PARSER::A )
                                       (vterm 'CLOVER.PARSER::X)))
                        (literal t
                                 'CLOVER.PARSER::=
                                 (list (constant 'CLOVER.PARSER::B )
                                       (constant 'CLOVER.PARSER::B )))
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
                                 (list (constant 'CLOVER.PARSER::A )
                                       (vterm 'CLOVER.PARSER::X)))
                        (literal t
                                 'CLOVER.PARSER::=
                                 (list (constant 'CLOVER.PARSER::B )
                                       (constant 'CLOVER.PARSER::B )))
                        (equation nil
                              (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::X))))
                        ))))

      (is 
        (clause=
          (parse-conseq-logical-expression "[A, B] = cons(A,cons(B,NIL))")
          (clause (list 
                    (literal t
                             'CLOVER.PARSER::=
                             (list 
                               (fterm 'CLOVER.PARSER::CONS 
                                      (list (constant 'CLOVER.PARSER::A )
                                            (fterm 'CLOVER.PARSER::CONS 
                                                   (list (constant 'CLOVER.PARSER::B )  (constant 'CLOVER.PARSER::NIL )))))
                               (fterm 'CLOVER.PARSER::CONS 
                                      (list (constant 'CLOVER.PARSER::A )
                                            (fterm 'CLOVER.PARSER::CONS 
                                                   (list (constant 'CLOVER.PARSER::B )  (constant 'CLOVER.PARSER::NIL )))))))))))

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
                               (constant 'CLOVER.PARSER::A )
                               (constant 'CLOVER.PARSER::B )))
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
                               (fterm 'CLOVER.PARSER::S (list (constant 'CLOVER.PARSER::ZERO )))
                               (constant 'CLOVER.PARSER::ONE )))
                    (equation nil
                              (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                              (constant 'CLOVER.PARSER::A ))
                    (literal nil
                             'CLOVER.PARSER::PRED
                             (list 
                               (vterm 'CLOVER.PARSER::X)
                               (vterm 'CLOVER.PARSER::Y)))))))

      (is 
        (clause=
          (parse-premise-logical-expression "[A, B] = cons(A,cons(B,NIL))")
          (clause (list 
                    (literal nil
                             'CLOVER.PARSER::=
                             (list 
                               (fterm 'CLOVER.PARSER::CONS 
                                      (list (constant 'CLOVER.PARSER::A )
                                            (fterm 'CLOVER.PARSER::CONS 
                                                   (list (constant 'CLOVER.PARSER::B )  (constant 'CLOVER.PARSER::NIL )))))
                               (fterm 'CLOVER.PARSER::CONS 
                                      (list (constant 'CLOVER.PARSER::A )
                                            (fterm 'CLOVER.PARSER::CONS 
                                                   (list (constant 'CLOVER.PARSER::B )  (constant 'CLOVER.PARSER::NIL )))))))))))

      (is 
        (clause=
          (parse-premise-logical-expression "[A, B] = cons(A,cons(B,NIL)) | pred(x,y)")
          (clause (list 
                    (literal nil
                             'CLOVER.PARSER::PRED
                             (list 
                               (vterm 'CLOVER.PARSER::X)
                               (vterm 'CLOVER.PARSER::Y)))
                    (literal nil
                             'CLOVER.PARSER::=
                             (list 
                               (fterm 'CLOVER.PARSER::CONS 
                                      (list (constant 'CLOVER.PARSER::A )
                                            (fterm 'CLOVER.PARSER::CONS 
                                                   (list (constant 'CLOVER.PARSER::B )  (constant 'CLOVER.PARSER::NIL )))))
                               (fterm 'CLOVER.PARSER::CONS 
                                      (list (constant 'CLOVER.PARSER::A )
                                            (fterm 'CLOVER.PARSER::CONS 
                                                   (list (constant 'CLOVER.PARSER::B )  (constant 'CLOVER.PARSER::NIL )))))))))))

      )



(test clover.tests.parser.parse-mkbtt-expression.test1
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR x)(RULES hoge(x) -> foo(x))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (vterm 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR y)(RULES hoge(x) -> foo(x))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR X)(RULES hoge(x) -> foo(x))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR X)(RULES hoge() -> foo())"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (constant 'CLOVER.PARSER::HOGE)
                              (constant 'CLOVER.PARSER::FOO))))))
          (equation-set= result expected)))
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR x)(RULES hoge(x,a) -> foo(x,b))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::A)
                                           ))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::B)
                                           )))))))
          (equation-set= result expected)))
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR x y)(RULES hoge(x,a) -> foo(x,b))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::A)
                                           ))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::B)
                                           )))))))
          (equation-set= result expected)))
      (is
        (let ((result
                (parse-mkbtt-expression "(VAR x a)(RULES hoge(x,a) -> foo(x,b))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::A)
                                           ))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::B)
                                           )))))))
          (equation-set= result expected))))      


(test clover.tests.parser.parse-mkbtt-expression.test2

      (is
        (let ((result
                (parse-mkbtt-expression 
                  "(VAR x a)(RULES hoge(x,a,0()) -> f_oo_(x, b))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::A)
                                           (constant 'CLOVER.PARSER::0)))
                              (fterm 'CLOVER.PARSER::F_OO_
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::B))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression 
                  "(VAR x a)(RULES HOGE(x,a,0()) -> F_OO_(x, b))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::A)
                                           (constant 'CLOVER.PARSER::0)))
                              (fterm 'CLOVER.PARSER::F_OO_
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::B))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression 
                  "(VAR x a)(RULES Hoge(x,a,0()) -> F_oo_(x, b))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::A)
                                           (constant 'CLOVER.PARSER::0)))
                              (fterm 'CLOVER.PARSER::F_OO_
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (constant 'CLOVER.PARSER::B))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression "(VAR X)(RULES hoge(X) -> foo(x))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression "(VAR x)(RULES HOGE(x) -> foo(X))"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))
      )


(test clover.tests.parser.parse-mkbtt-expression.test3
      (is
        (let ((result
                (parse-mkbtt-expression
                  "(VAR X y)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))
)









(test clover.tests.parser.parse-mkbtt-expression.test4

      (is
        (let ((result
                (parse-mkbtt-expression
                  "(VAR X y)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )
                          (COMMENT example Z22 from
Avenhaus, Denzinger 93: Distributing equational theorem proving)
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression
                  "(VAR X y)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )
                          (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression
                  "(COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                  (VAR X y)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression
                  "
                  (VAR X y)
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))

    (signals expr-parse-error
        (let ((result
                (parse-mkbtt-expression
                  "
                  (VAR X y)
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )
                  (VAR z w)
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression
                  "
                  (VAR X y)
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   (RULES hoge(X) -> foo(x)
                          HOGE(x()) -> bar(X,y)
                          )
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::BAR
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::FOO
                                     (list (constant 'CLOVER.PARSER::X))))))))
          (equation-set= result expected))))



(test clover.tests.parser.parse-mkbtt-expression.test5
      (is
        (let ((result
                (parse-mkbtt-expression
                  "(VAR)(RULES x -> y A -> B)"))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (constant 'CLOVER.PARSER::X)
                              (constant 'CLOVER.PARSER::y))
                    (equation nil 
                              (constant 'CLOVER.PARSER::A)
                              (constant 'CLOVER.PARSER::B))))))
          (equation-set= result expected)))

      (is
        (let ((result
                (parse-mkbtt-expression
                  "
                  (VAR X y)
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   (RULES hoge(X) -> .(x, -(X))
                          HOGE(x()) -> /(X,y)
                          )
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::/
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::.
                                     (list (constant 'CLOVER.PARSER::X)
                                           (fterm 'CLOVER.PARSER::-
                                                  (list (vterm 'CLOVER.PARSER::X))))))))))
          (equation-set= result expected)))


          (is
        (let ((result
                (parse-mkbtt-expression
                  "
                  (VAR X y)
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   (RULES hoge(X) -> @(x, &(X))
                          HOGE(x()) -> \\(X,y)
                          )
                  (COMMENT example Z22 from Avenhaus, Denzinger (93): Distributing equational theorem proving)
                   "))
              (expected
                (equation-set
                  (list
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (constant 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::\\
                                     (list (vterm 'CLOVER.PARSER::X)
                                           (vterm 'CLOVER.PARSER::Y))))
                    (equation nil 
                              (fterm 'CLOVER.PARSER::HOGE
                                     (list (vterm 'CLOVER.PARSER::X)))
                              (fterm 'CLOVER.PARSER::@
                                     (list (constant 'CLOVER.PARSER::X)
                                           (fterm 'CLOVER.PARSER::&
                                                  (list (vterm 'CLOVER.PARSER::X))))))))))
          (equation-set= result expected)))

          )
