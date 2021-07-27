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


(test clover.tests.util.term<.test1
      (setf *term-order-algorithm* :original)
      (is (term< (vterm 'x)
                 (vterm 'y)
                 *term-order-algorithm*)) 
      (is (not (term< (vterm 'y)
                 (vterm 'x)
                 *term-order-algorithm*)))
      (is (term< (vterm 'x) 
                 (fterm 'f (list (vterm 'x))) 
                 *term-order-algorithm*))
      (is (not (term< (fterm 'f (list (vterm 'x)))
                 (vterm 'x) 
                 *term-order-algorithm*)))
      (is (term< (constant 'HOGE ) 
                 (fterm 'f (list (vterm 'x))) 
                 *term-order-algorithm*))
      (is (not (term< 
                 (fterm 'f (list (vterm 'x)))
                 (constant'HOGE ) 
                 *term-order-algorithm*)))
      (is (term< (constant 'ABC )
                 (constant 'DEF )
                 *term-order-algorithm*))
      (is (not (term< 
                 (constant 'DEF )
                 (constant 'ABC )
                 *term-order-algorithm*)))

      ;; same function symbol
      (is (term< (fterm 'f (list (constant 'A ))) 
                 (fterm 'f (list (vterm 'x))) 
                 *term-order-algorithm*))
      (is (not (term< (fterm 'f (list (vterm 'x)))
                      (fterm 'f (list (constant 'A ))) 
                 *term-order-algorithm*)))
      (is (term< (fterm 'f (list (vterm 'x))) 
                 (fterm 'f (list (fterm 'g (list (vterm 'x))))) 
                 *term-order-algorithm*))
      (is (not (term< 
                 (fterm 'f (list (fterm 'g (list (vterm 'x)))))
                 (fterm 'f (list (vterm 'x))) 
                 *term-order-algorithm*)))
      (is (term< (fterm 'f (list (vterm 'x) (vterm 'y))) 
                 (fterm 'f (list (fterm 'g (list (vterm 'x))) (vterm 'z))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (constant 'A ))) 
                 (fterm 'f (list (fterm 'g (list (vterm 'x))) (vterm 'z))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (constant 'A ))) 
                 (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'x))))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (vterm 'y) (constant 'A ))) 
                 (fterm 'f (list (vterm 'x) (vterm 'y) (vterm 'z)))
                 *term-order-algorithm*))

      ;; difference function symbol
      (is (term< (fterm 'f (list (constant 'A ))) 
                 (fterm 'h (list (vterm 'x))) 
                 *term-order-algorithm*))
      (is (not (term< (fterm 'f (list (vterm 'x)))
                      (fterm 'h (list (constant 'A ))) 
                 *term-order-algorithm*)))
      (is (term< (fterm 'f (list (vterm 'x))) 
                 (fterm 'h (list (fterm 'g (list (vterm 'x))))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'h (list (vterm 'x)))
                 (fterm 'f (list (fterm 'g (list (vterm 'x)))))
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (vterm 'y))) 
                 (fterm 'h (list (fterm 'g (list (vterm 'x))) (vterm 'z))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (constant 'A ))) 
                 (fterm 'h (list (fterm 'g (list (vterm 'x))) (vterm 'z))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (constant 'A ))) 
                 (fterm 'h (list (vterm 'z) (fterm 'g (list (vterm 'x))))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'f (list (vterm 'x) (vterm 'y) (constant 'A ))) 
                 (fterm 'h (list (vterm 'x) (vterm 'y) (vterm 'z)))
                 *term-order-algorithm*))
      (is (term< (fterm 'h (list (constant 'A ))) 
                 (fterm 'g (list (vterm 'x))) 
                 *term-order-algorithm*))
      (is (not (term< (fterm 'h (list (vterm 'x)))
                      (fterm 'g (list (constant 'A ))) 
                 *term-order-algorithm*)))
      (is (term< (fterm 'h (list (vterm 'x))) 
                 (fterm 'g (list (fterm 'g (list (vterm 'x))))) 
                 *term-order-algorithm*))
      (is (not (term< 
                 (fterm 'h (list (fterm 'g (list (vterm 'x)))))
                 (fterm 'g (list (vterm 'x))) 
                 *term-order-algorithm*)))
      (is (term< (fterm 'h (list (vterm 'x) (vterm 'y))) 
                 (fterm 'j (list (fterm 'g (list (vterm 'x) (vterm 'w))) (vterm 'z))) 
                 *term-order-algorithm*))
      (is (term< (fterm 'h (list (vterm 'x) (constant 'A ))) 
                 (fterm 'k (list (fterm 'g (list (vterm 'x) (vterm 'w))) (vterm 'z))) 
                 *term-order-algorithm*))

      (is (term< (fterm 'h (list (vterm 'x) (constant 'A ))) 
                 (fterm 'l (list (vterm 'z) (fterm 'g (list (vterm 'x) (vterm 'w)))))
                 *term-order-algorithm*))

      (is (not (term< (fterm 'h (list (vterm 'x) (vterm 'y) (constant'A ))) 
                 (fterm 'g (list (vterm 'x) (vterm 'y) (vterm 'z)))
                 *term-order-algorithm*)))

      (is (term< (vterm 'x)
                 (fterm 'plus (list (constant 'ZERO ) (vterm 'x)))
                 *term-order-algorithm*)) 

      (is (term< 
            (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
            (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))
            *term-order-algorithm*))

      )

(test clover.tests.util.term<.test2

      (setf *term-order-algorithm* :lpo)

      (is (term< (vterm 'x)
                 (fterm 'plus (list (constant 'ZERO ) (vterm 'x)))
                 *term-order-algorithm*)) 

      (is (term< (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                 *term-order-algorithm*))

      (is (term< (vterm 'x)
                 (fterm 'cons (list (vterm 'x) (vterm 'y)))
                 *term-order-algorithm*))
      )

;(test clover.tests.util.term<.test3
;      (setf *term-order-algorithm* :lpo)
;      (let ((target
;              (rewrite-rule-set
;                 (list
;                   (rewrite-rule
;                     (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
;                     (vterm 'x))
;                   (rewrite-rule
;                     (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'x)))
;                     (constant 'ZERO))
;                   (rewrite-rule
;                     (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
;                     (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z))))))
;                   (rewrite-rule
;                     (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (fterm 'plus (list (vterm 'x) (vterm 'y)))))
;                     (vterm 'y))
;                   (rewrite-rule
;                     (fterm 'plus (list (vterm 'x) (constant 'ZERO)))
;                     (vterm 'x))
;                   (rewrite-rule
;                     (fterm 'inv (list (fterm 'inv (list (vterm 'x)))))
;                     (vterm 'x))
;                   (rewrite-rule
;                     (fterm 'plus (list (vterm 'x) (fterm 'inv (list (vterm 'x)))))
;                     (constant 'ZERO))
;                   (rewrite-rule
;                     (fterm 'inv (list (constant 'ZERO)))
;                     (constant 'ZERO))
;                   (rewrite-rule
;                     (fterm 'plus (list (vterm 'x) (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'y)))))
;                     (vterm 'y))
;                   (rewrite-rule
;                     (fterm 'inv (list (fterm 'plus (list (vterm 'x) (vterm 'y)))))
;                     (fterm 'plus (list (fterm 'inv (list (vterm 'y)))
;                                        (fterm 'inv (list (vterm 'x))))))))))
;        (loop
;          :for rule :in (rewrite-rule-set.rewrite-rules target)
;          :for src := (rewrite-rule.src rule)
;          :for dst := (rewrite-rule.dst rule)
;          :do (is (term< dst src *term-order-algorithm*)))))

;(test clover.tests.util.term<.test4
;      (setf *term-order-algorithm* :lpo)
;      (let ((target
;              (rewrite-rule-set
;                (list
;                  (rewrite-rule
;                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
;                    (vterm 'x))
;                  (rewrite-rule
;                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
;                                                            (vterm 'y)))
;                                         (vterm 'z)))
;                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
;                  (rewrite-rule
;                    (fterm 'reverse (list (constant 'NIL)))
;                    (constant 'NIL))
;                  (rewrite-rule
;                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
;                    (vterm 'x)
;                    )
;                  (rewrite-rule
;                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
;                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
;                                         (fterm 'cons (list (vterm 'x) (constant 'NIL))))))
;                  (rewrite-rule
;                    (fterm 'reverse (list (fterm 'append (list (vterm 'x) (fterm 'cons (list (vterm 'y) (constant 'NIL)))))))
;                    (fterm 'cons (list (vterm 'y) (fterm 'reverse (list (vterm 'x))))))))
;              ))
;        (loop
;          :for rule :in (rewrite-rule-set.rewrite-rules target)
;          :for src := (rewrite-rule.src rule)
;          :for dst := (rewrite-rule.dst rule)
;          :do (is (term< dst src *term-order-algorithm*)))))
