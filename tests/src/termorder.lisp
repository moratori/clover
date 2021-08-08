(defpackage clover.tests.termorder
  (:use :cl
        :clover.types
        :clover.util
        :clover.termorder
        :1am)
  (:import-from :clover.property
                :*term-order-algorithm*)
  )
(in-package :clover.tests.termorder)


(test clover.tests.termorder.term<.test2

      (setf *term-order-algorithm* :lpo)

      (let ((ordering1
              (function-symbol-ordering
                (list 'zero
                      'plus
                      'inv)))
            (ordering2
              (function-symbol-ordering
                (list 'nil 'cons 'append 'reverse))))

        (is (term< (fterm 'plus (list (vterm 'y) (vterm 'z)))
                   (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                   ordering1
                   *term-order-algorithm*))
        (is (term< (vterm 'x)
                   (fterm 'plus (list (constant 'ZERO ) (vterm 'x)))
                   ordering1
                   *term-order-algorithm*))
        (is (term< (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                   (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                   ordering1
                   *term-order-algorithm*))
        (is (term< (vterm 'x)
                   (fterm 'cons (list (vterm 'x) (vterm 'y)))
                   ordering2
                   *term-order-algorithm*))
        (is (term< (fterm 'append (list (fterm 'reverse (list (vterm 'y))) 
                                        (fterm 'cons (list (vterm 'x) (constant 'NIL)))))
                   (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
                   ordering2
                   *term-order-algorithm*))))


(test clover.tests.termorder.term<.test3
      (setf *term-order-algorithm* :lpo)
      (let ((ordering
              (function-symbol-ordering
                (list 'zero
                      'plus
                      'inv)))
            (target
              (rewrite-rule-set
                 (list
                   (rewrite-rule
                     (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'x)))
                     (constant 'ZERO))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                     (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z))))))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (fterm 'plus (list (vterm 'x) (vterm 'y)))))
                     (vterm 'y))
                   (rewrite-rule
                     (fterm 'plus (list (vterm 'x) (constant 'ZERO)))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'inv (list (fterm 'inv (list (vterm 'x)))))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'plus (list (vterm 'x) (fterm 'inv (list (vterm 'x)))))
                     (constant 'ZERO))
                   (rewrite-rule
                     (fterm 'inv (list (constant 'ZERO)))
                     (constant 'ZERO))
                   (rewrite-rule
                     (fterm 'plus (list (vterm 'x) (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'y)))))
                     (vterm 'y))
                   (rewrite-rule
                     (fterm 'inv (list (fterm 'plus (list (vterm 'x) (vterm 'y)))))
                     (fterm 'plus (list (fterm 'inv (list (vterm 'y)))
                                        (fterm 'inv (list (vterm 'x))))))))))
        (loop
          :for rule :in (rewrite-rule-set.rewrite-rules target)
          :for src := (rewrite-rule.src rule)
          :for dst := (rewrite-rule.dst rule)
          :do (is (term< dst src ordering *term-order-algorithm*)))))

(test clover.tests.termorder.term<.test4
      (setf *term-order-algorithm* :lpo)
      (let ((ordering
              (function-symbol-ordering
                (list 'nil 'cons 'append 'reverse)))
            (target
              (rewrite-rule-set
                (list
                  (rewrite-rule
                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
                                                            (vterm 'y)))
                                         (vterm 'z)))
                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
                  (rewrite-rule
                    (fterm 'reverse (list (constant 'NIL)))
                    (constant 'NIL))
                  (rewrite-rule
                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
                    (vterm 'x)
                    )
                  (rewrite-rule
                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                         (fterm 'cons (list (vterm 'x) (constant 'NIL))))))
                  (rewrite-rule
                    (fterm 'reverse (list (fterm 'append (list (vterm 'x) (fterm 'cons (list (vterm 'y) (constant 'NIL)))))))
                    (fterm 'cons (list (vterm 'y) (fterm 'reverse (list (vterm 'x))))))))
              ))
        (loop
          :for rule :in (rewrite-rule-set.rewrite-rules target)
          :for src := (rewrite-rule.src rule)
          :for dst := (rewrite-rule.dst rule)
          :do (is (term< dst src ordering *term-order-algorithm*)))))


(test clover.tests.termorder.term<.test5
      (setf *term-order-algorithm* :lpo)
      (let ((ordering
              (function-symbol-ordering
                (list 
                  'zero 
                  'plus
                  'inv))
              ))
        (is (or
            (term< 
              (constant 'ZERO)
              (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                 (vterm 'x)))
              ordering
              *term-order-algorithm*)
            (term< 
              (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                 (vterm 'x)))
              (constant 'ZERO)
              ordering
              *term-order-algorithm*)))
        )
      
      )

(test clover.tests.termorder.term<.test6
      (setf *term-order-algorithm* :lpo)

      (let ((ordering
              (function-symbol-ordering
                (list 
                  'zero 
                  'plus
                  'inv))))

      (is (term< 
            (vterm 'x)
            (fterm 'plus (list (constant 'ZERO)
                               (vterm 'x)))
            ordering
            *term-order-algorithm*))

      (is (term< 
            (constant 'ZERO)
            (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                               (vterm 'x)))
            ordering
            *term-order-algorithm*))

      (is (term< 
            (fterm 'plus (list (Vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))
            (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
            ordering
            *term-order-algorithm*))))
