(defpackage clover.tests.clover
  (:use :cl
        :clover.property
        :clover.clover
        :clover.types
        :clover.resolution
        :1am))
(in-package :clover.tests.clover)


(test clover.tests.clover.finish
         
        (is (clover.clover::finish 
              (clause-set (list (clause nil) 
                                (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred3 nil)))))))
        (is (clover.clover::finish 
              (clause-set (list  (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                 (clause nil)
                                 (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                               (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                               (literal nil 'Pred3 nil)))))))
        
        (is (not (clover.clover::finish (clause-set (list (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                                                          (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                                        (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                                                        (literal nil 'Pred3 nil))))))))
        )


(test clover.tests.clover.start_resolution.exhaustive
      (let ((linear-sucess
             (list 
               (clause-set (list  (clause (list (literal t 'P nil)) nil nil nil nil :goal)
                                  (clause (list (literal nil 'P nil)))))
               (clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                  (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                                  (clause (list (literal t 'P nil)) nil nil nil nil :goal)))
               (clause-set (list  (clause (list (literal nil 'P nil)))
                                  (clause (list (literal t 'P nil) (literal nil 'Q nil)))
                                  (clause (list (literal t 'Q nil)) nil nil nil nil :goal)))
               (clause-set (list  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'x)))))
                                  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'y)))))
                                  (clause (list (literal nil 'R (list (fterm 'A nil)
                                                                      (fterm 'B nil)))))
                                  (clause (list (literal nil 'R (list (fterm 'B nil)
                                                                      (fterm 'A nil)))))
                                  (clause (list (literal t 'P (list (vterm 'x)))) nil nil nil nil :goal)))
               (clause-set (list  (clause (list (literal nil 'LEN (list (fterm 'NIL nil)
                                                                        (fterm 'ZERO nil)))))
                                  (clause (list (literal t 'LEN (list (vterm 'c)
                                                                      (vterm 'n)))
                                                (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                                           (vterm 'c)))
                                                                        (fterm 'SUCC (list (vterm 'n)))))))
                                  (clause (list (literal t 'LEN (list (fterm 'CONS (list (fterm 'A nil) 
                                                                                         (fterm 'CONS (list (fterm 'B nil) (fterm 'NIL nil)))))
                                                                      (vterm 'x)))) nil nil nil nil :goal))))))
  (setf *resolution-algorithm* :exhaustive)
  (setf *save-resolution-history* nil)
  (loop :for each :in linear-sucess
        :do (is (start_resolution each)))))






(test clover.tests.clover.start_resolution.linear

     (let ((linear-sucess
             (list 
               (clause-set (list  (clause (list (literal t 'P nil)) nil nil nil nil :goal)
                                  (clause (list (literal nil 'P nil)))))
               (clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                  (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                                  (clause (list (literal t 'P nil)) nil nil nil nil :goal)))
               (clause-set (list  (clause (list (literal nil 'P nil)))
                                  (clause (list (literal t 'P nil) (literal nil 'Q nil)))
                                  (clause (list (literal t 'Q nil)) nil nil nil nil :goal)))
               (clause-set (list  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'x)))))
                                  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'y)))))
                                  (clause (list (literal nil 'R (list (fterm 'A nil)
                                                                      (fterm 'B nil)))))
                                  (clause (list (literal nil 'R (list (fterm 'B nil)
                                                                      (fterm 'A nil)))))
                                  (clause (list (literal t 'P (list (vterm 'x)))) nil nil nil nil :goal)))
               (clause-set (list  (clause (list (literal nil 'LEN (list (fterm 'NIL nil)
                                                                        (fterm 'ZERO nil)))))
                                  (clause (list (literal t 'LEN (list (vterm 'c)
                                                                      (vterm 'n)))
                                                (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                                           (vterm 'c)))
                                                                        (fterm 'SUCC (list (vterm 'n)))))))
                                  (clause (list (literal t 'LEN (list (fterm 'CONS (list (fterm 'A nil) 
                                                                                         (fterm 'CONS (list (fterm 'B nil) (fterm 'NIL nil)))))
                                                                      (vterm 'x)))) nil nil nil nil :goal)))
;               (clause-set (list  (clause (list (literal nil 'P (list (vterm 'x)
;                                                                      (fterm 'E nil)
;                                                                      (vterm 'x)))))
;                                  (clause (list (literal nil 'P (list (fterm 'E nil)
;                                                                      (vterm 'x)
;                                                                      (vterm 'x)))))
;                                  (clause (list (literal nil 'P (list (vterm 'u)
;                                                                      (vterm 'z)
;                                                                      (vterm 'w)))
;                                                (literal t   'P (list (vterm 'y)
;                                                                      (vterm 'z)
;                                                                      (vterm 'v)))
;                                                (literal t   'P (list (vterm 'x)
;                                                                      (vterm 'y)
;                                                                      (vterm 'u)))
;                                                (literal t   'P (list (vterm 'x)
;                                                                      (vterm 'v)
;                                                                      (vterm 'w)))))
;                                  (clause (list (literal nil 'P (list (vterm 'x)
;                                                                      (vterm 'v)
;                                                                      (vterm 'w)))
;                                                (literal t   'P (list (vterm 'y)
;                                                                      (vterm 'z)
;                                                                      (vterm 'v)))
;                                                (literal t   'P (list (vterm 'x)
;                                                                      (vterm 'y)
;                                                                      (vterm 'u)))
;                                                (literal t   'P (list (vterm 'u)
;                                                                      (vterm 'z)
;                                                                      (vterm 'w)))))
;                                  (clause (list (literal nil 'P (list (vterm 'x)
;                                                                      (vterm 'x)
;                                                                      (fterm 'E nil)))))
;                                  (clause (list (literal nil 'P (list (fterm 'A nil)
;                                                                      (fterm 'B nil)
;                                                                      (fterm 'C nil)))))
;                                  (clause (list (literal t   'P (list (fterm 'B nil)
;                                                                      (fterm 'A nil)
;                                                                      (fterm 'C nil)))) nil nil nil nil :goal)))
               )))
  (setf *resolution-algorithm* :linear)
  (setf *save-resolution-history* nil)
  (loop :for each :in linear-sucess
        :do (is (start_resolution each)))))


