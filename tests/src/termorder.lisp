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

      (setf *term-order-algorithm* :dictionary)

      (is (term< (vterm 'x)
                 (fterm 'plus (list (constant 'ZERO ) (vterm 'x)))
                 *term-order-algorithm*)) 

      (is (term< (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                 (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                 *term-order-algorithm*
                 ))
      )

