(defpackage clover.tests.util
  (:use :cl
        :clover.types
        :clover.util
        :1am))
(in-package :clover.tests.util)



(test clover.tests.util.term=
      (is (term= (vterm 'x) (vterm 'x)))
      (is (not (term= (vterm 'x) (vterm 'y))))
      (is (term= (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y))))) 
                 (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))))
      (is (not (term= (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y))))) 
                      (fterm 'f (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
      )


(test clover.tests.util.clause-subset.test1
      (is (clause-subset
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))))
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))
                (literal t   'R (list (vterm 'x)))))))
      (is (clause-subset
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))))
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))
                (literal nil 'R (list (vterm 'x)))))))
      (is (clause-subset
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                (literal nil 'Q (list (vterm 'x)))))
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                (literal nil 'Q (list (vterm 'x)))
                (literal t   'R (list (vterm 'x)))))))
      (is (clause-subset
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                (literal nil 'Q (list (vterm 'x)))))
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                (literal nil 'Q (list (vterm 'x)))
                (literal nil 'R (list (vterm 'x)))))))
      )

(test clover.tests.util.clause-subset.test2
      (is (not (clause-subset
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))))
            (clause 
              (list
                (literal nil 'P (list (vterm 'y) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))
                (literal t   'R (list (vterm 'x))))))))
      (is (not (clause-subset
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'z)))))
            (clause 
              (list
                (literal nil 'P (list (vterm 'x) (vterm 'y)))
                (literal nil 'Q (list (vterm 'x)))
                (literal nil 'R (list (vterm 'x))))))))
      (is (not (clause-subset
                 (clause 
                   (list
                     (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                     (literal nil 'Q (list (vterm 'x)))))
                 (clause 
                   (list
                     (literal nil 'S (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                     (literal nil 'T (list (vterm 'x)))
                     (literal t   'R (list (vterm 'x))))))))
      (is (not (clause-subset
                 (clause 
                   (list
                     (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'y)))))
                     (literal nil 'Q (list (vterm 'x)))))
                 (clause 
                   (list
                     (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'z)))))
                     (literal nil 'Q (list (vterm 'x)))
                     (literal nil 'R (list (vterm 'x))))))))
      )


(test clover.tests.util.literal=

      (is (literal= (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                    (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))

      (is (not (literal= (literal nil 'P (list (vterm 'x) (vterm 'w) (vterm 'z)))
                         (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))))
      
      (is (not (literal= (literal t 'P (list (vterm 'x) (vterm 'w) (vterm 'z)))
                         (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))))

      (is (not (literal= (literal nil 'P (list (vterm 'x) (vterm 'w) (vterm 'z) (fterm 'f (list (vterm 'v)))))
                         (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))))
      )
