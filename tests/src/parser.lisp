(defpackage clover.tests.parser
  (:use :cl
        :clover.property
        :clover.types
        :clover.parser
        :clover.util
        :1am))
(in-package :clover.tests.parser)


(test clover.tests.parser.parse-premise-logical-expression
      
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



(test clover.tests.parser.parse-conseq-logical-expression
      
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


(test clover.tests.parser.parse-equation-logical-expression.test1

      (is 
        (literal=
          (parse-equation-logical-expression "A = A")
          (literal nil
                   'CLOVER.PARSER::=
                   (list (fterm 'CLOVER.PARSER::A nil)
                         (fterm 'CLOVER.PARSER::A nil)))))
      (is 
        (literal=
          (parse-equation-logical-expression "A = x")
          (literal nil
                   'CLOVER.PARSER::=
                   (list (fterm 'CLOVER.PARSER::A nil)
                         (vterm 'CLOVER.PARSER::X)))))

      (is 
        (literal=
          (parse-equation-logical-expression "x = A")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (fterm 'CLOVER.PARSER::A nil)))))

      (is 
        (literal=
          (parse-equation-logical-expression "x = y")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::X)
                     (vterm 'CLOVER.PARSER::Y)))))

      (is 
        (literal=
          (parse-equation-logical-expression "f(x) = g(y)")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                     (fterm 'CLOVER.PARSER::G (list (vterm 'CLOVER.PARSER::Y)))))))

      (is 
        (literal=
          (parse-equation-logical-expression "f(x) = z")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))
                     (vterm 'CLOVER.PARSER::Z)))))

      (is 
        (literal=
          (parse-equation-logical-expression "z = f(x)")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (vterm 'CLOVER.PARSER::Z)
                     (fterm 'CLOVER.PARSER::F (list (vterm 'CLOVER.PARSER::X)))))))

      (is 
        (literal=
          (parse-equation-logical-expression "s(ZERO) = ONE")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))
                     (fterm 'CLOVER.PARSER::ONE nil)))))

      (is 
        (literal=
          (parse-equation-logical-expression "ONE = s(ZERO)")
          (literal nil
                   'CLOVER.PARSER::=
                   (list 
                     (fterm 'CLOVER.PARSER::ONE nil)
                     (fterm 'CLOVER.PARSER::S (list (fterm 'CLOVER.PARSER::ZERO nil)))))))

      )


