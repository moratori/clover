(defpackage clover.tests.unify
  (:use :cl
        :clover.conditions
        :clover.unify
        :clover.types
        :clover.util
        :1am))
(in-package :clover.tests.unify)



(test clover.tests.unify.%collect-unifier-set-candidate 
      (is 
        (let ((us 
                (clover.unify::%collect-unifier-set-candidate 
                  (vterm 'x) (vterm 'y))))
          (unifier-set= 
            us
            (unifier-set 
              (list (unifier (vterm 'x) (vterm 'y)))))))

      (is 
        (let ((us 
                (clover.unify::%collect-unifier-set-candidate 
                  (vterm 'x) (fterm 'f (list (vterm 'z))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'f (list (vterm 'z)))))))))

      (is 
        (let ((us 
                (clover.unify::%collect-unifier-set-candidate 
                  (fterm 'f (list (vterm 'x))) (fterm 'f (list (vterm 'z))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'z)))))))
      
      (is 
        (let ((us 
                (clover.unify::%collect-unifier-set-candidate 
                  (fterm 'f (list (vterm 'x))) (fterm 'f (list (fterm 'g (list (vterm 'z))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'g (list (vterm 'z)))))))))
      
      (is 
        (let ((us 
                (clover.unify::%collect-unifier-set-candidate 
                  (fterm 'f (list (vterm 'x) 
                                  (vterm 'y))) 
                  (fterm 'f (list (fterm 'g (list (vterm 'z))) 
                                  (fterm 'h (list (vterm 'w))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'g (list (vterm 'z))))
                    (unifier (vterm 'y) (fterm 'h (list (vterm 'w)))))))))
       
      (is 
        (handler-case
            (clover.unify::%collect-unifier-set-candidate 
              (vterm 'x) (fterm 'f (list (vterm 'x))))
            (occurrence-check-error (e)
              t)))
      
      (is 
        (handler-case
            (clover.unify::%collect-unifier-set-candidate 
              (fterm 'f (list (vterm 'x))) (fterm 'f (list (vterm 'x))))
            (occurrence-check-error (e)
              t)))

      (is 
        (handler-case
            (clover.unify::%collect-unifier-set-candidate 
              (fterm 'g (list (vterm 'x))) (fterm 'f (list (vterm 'z))))
            (unmatching-fterm-error (e)
              t)))

      )

