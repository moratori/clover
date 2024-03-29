(defpackage clover.tests.clover
  (:use :cl
        :clover.property
        :clover.clover
        :clover.types
        :clover.resolution
        :1am)
  )
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



(defparameter *simple-test-clause-set* (list 
(clause-set (list  (clause (list (literal t 'P nil))
                           nil nil nil :conseq)
                                  (clause (list (literal nil 'P nil)))))

(clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                   (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                   (clause (list (literal t 'P nil))
                           nil nil nil :conseq
                           )))

(clause-set (list  (clause (list (literal nil 'P nil)))
                                  (clause (list (literal t 'P nil) (literal nil 'Q nil)))
                                  (clause (list (literal t 'Q nil))
                                          nil nil nil :conseq)))
(clause-set (list  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'x)))))
                                  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'y)))))
                                  (clause (list (literal nil 'R (list (constant 'A )
                                                                      (constant 'B )))))
                                  (clause (list (literal nil 'R (list (constant 'B )
                                                                      (constant 'A )))))
                                  (clause (list (literal t 'P (list (vterm 'x))))
                                          nil nil nil :conseq)))
(clause-set (list  (clause (list (literal nil 'LEN (list (constant 'NIL )
                                                                        (constant 'ZERO )))))
                                  (clause (list (literal t 'LEN (list (vterm 'c)
                                                                      (vterm 'n)))
                                                (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                                           (vterm 'c)))
                                                                        (fterm 'SUCC (list (vterm 'n)))))))
                                  (clause (list (literal t 'LEN (list (fterm 'CONS (list (constant 'A) 
                                                                                         (fterm 'CONS (list (constant 'B) (constant 'NIL)))))
                                                                      (vterm 'x))))
                                          nil nil nil :conseq)))
(clause-set (list  (clause (list (literal nil 'P (list (vterm 'u)
                                                                      (vterm 'z)
                                                                      (vterm 'w)))
                                                (literal t   'P (list (vterm 'w)
                                                                      (vterm 'z)
                                                                      (vterm 'u)))))
                                  (clause (list (literal nil 'P (list (constant 'C )
                                                                      (constant 'A )
                                                                      (constant 'B )))))
                                  (clause (list (literal t   'P (list (constant 'B )
                                                                      (constant 'A )
                                                                      (constant 'C ))))
                                          nil nil nil :conseq)))
(clause-set (list (clause (list (literal t   'P (list (constant 'B )
                                                                  (constant 'A )
                                                                  (constant 'C ))))
                          nil nil nil :conseq)
                              (clause (list (literal nil 'P (list (vterm 'x)
                                                                  (vterm 'x)
                                                                  (constant 'E )))))
                              (clause (list (literal nil 'P (list (constant 'C )
                                                                  (vterm 'z)
                                                                  (constant 'A )))
                                            (literal t   'P (list (constant 'B )
                                                                  (vterm 'z)
                                                                  (constant 'E )))))
                              (clause (list (literal nil 'P (list (vterm 'x)
                                                                  (vterm 'v)
                                                                  (vterm 'w)))
                                            (literal t   'P (list (vterm 'x)
                                                                  (vterm 'w)
                                                                  (vterm 'v)))))
                              (clause (list (literal nil 'P (list (vterm 'u)
                                                                  (vterm 'z)
                                                                  (vterm 'w)))
                                            (literal t   'P (list (vterm 'w)
                                                                  (vterm 'z)
                                                                  (vterm 'u)))))
                              ))
(clause-set 
                    (list (clause 
                            (list (literal nil 'LEN (list (constant 'NIL)
                                                          (constant 'ZERO ))))
                            nil nil nil)
                          (clause 
                            (list (literal t 'LEN (list (vterm 'c)
                                                        (vterm 'n)))
                                  (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                             (vterm 'c)))
                                                          (fterm 'SUCC (list (vterm 'n))))))
                            nil nil nil)
                          (clause 
                            (list (literal t 'LEN (list (fterm 'CONS (list (constant 'A ) 
                                                                           (fterm 'CONS (list (constant 'B ) (constant 'NIL )))))
                                                        (vterm 'x)))) nil nil nil :conseq)))
(clause-set (list  (clause (list (literal t 'P (list (constant 'C )
                                                                 (constant 'B )
                                                                 (constant 'A ))))
                           nil nil nil :conseq) 
                               (clause (list (literal nil 'P (list (constant 'A )
                                                                   (constant 'B )
                                                                   (constant 'C )))))
                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (constant 'E )
                                                                   (vterm 'x)))))
                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (vterm 'x)
                                                                   (constant 'E )))))
                               (clause (list (literal nil 'P (list (vterm 'u)
                                                                   (vterm 'z)
                                                                   (vterm 'w)))
                                             (literal t   'P (list (vterm 'y)
                                                                   (vterm 'z)
                                                                   (vterm 'v)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'y)
                                                                   (vterm 'u)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'v)
                                                                   (vterm 'w)))))))
(clause-set (list  (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (constant 'E )
                                                                   (vterm 'x)))))
                               (clause (list (literal t 'P (list (constant 'B )
                                                                 (constant 'A )
                                                                 (constant 'C ))))
                                       nil nil nil :conseq) 
                               (clause (list (literal nil 'P (list (constant 'C )
                                                                   (constant 'A )
                                                                   (constant 'B )))))
                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (vterm 'x)
                                                                   (constant 'E)))))
                               (clause (list (literal nil 'P (list (vterm 'u)
                                                                   (vterm 'z)
                                                                   (vterm 'w)))
                                             (literal t   'P (list (vterm 'y)
                                                                   (vterm 'z)
                                                                   (vterm 'v)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'y)
                                                                   (vterm 'u)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'v)
                                                                   (vterm 'w)))))))
(clause-set (list  (clause (list (literal nil 'P (list (constant 'E )
                                                                   (vterm 'x)
                                                                   (vterm 'x)))))

                               (clause (list (literal nil 'P (list (vterm 'x )
                                                                   (vterm 'x )
                                                                   (constant 'E ))))) 

                               (clause (list (literal nil 'P (list (constant 'C )
                                                                   (constant 'B )
                                                                   (constant 'A )))))

                               (clause (list (literal t 'P (list (constant 'C )
                                                                 (constant 'A )
                                                                 (constant 'B ))))
                                       nil nil nil :conseq)

                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (vterm 'v)
                                                                   (vterm 'w)))
                                             (literal t   'P (list (vterm 'y)
                                                                   (vterm 'z)
                                                                   (vterm 'v)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'y)
                                                                   (vterm 'u)))
                                             (literal t   'P (list (vterm 'u)
                                                                   (vterm 'z)
                                                                   (vterm 'w)))))))

(clause-set (list  (clause (list (literal t 'member (list (vterm 'x) (constant 'NIL)))))
                   (clause (list (literal nil 'member (list (vterm 'x) (fterm 'cons (list (vterm 'x) (vterm 'y)))))))
                   (clause (list (literal t 'member (list (vterm 'x) 
                                                          (vterm 'y)))
                                 (literal nil 'member (list (vterm 'x) 
                                                            (fterm 'cons (list (vterm 'z) (vterm 'y)))))
                                 ))
                   (clause (list (literal t 'member (list (constant 'A)
                                                          (fterm 'cons (list (constant 'C)
                                                                             (fterm 'cons (list (constant 'B)
                                                                                                (fterm 'cons (list (constant 'A) (constant 'NIL))))))))))
                           nil nil nil :conseq
                           )))

))


(defparameter *difficult-test-clause-set* (list

(clause-set (list  
              (clause (list (literal t 'P (list (constant 'C )
                                                (constant 'A )
                                                (constant 'B ))))
                      nil nil nil :conseq)

              (clause (list (literal nil 'P (list (constant 'E )
                                                  (vterm 'x)
                                                  (vterm 'x)))))

              (clause (list (literal nil 'P (list (vterm 'x )
                                                  (vterm 'x )
                                                  (constant 'E ))))) 

              (clause (list (literal nil 'P (list (constant 'A)
                                                  (constant 'B )
                                                  (constant 'C)))))

              (clause (list (literal nil 'P (list (vterm 'x )
                                                  (constant 'E )
                                                  (vterm 'x )))))

              (clause (list (literal nil 'P (list (vterm 'x)
                                                  (vterm 'v)
                                                  (vterm 'w)))

                            (literal t   'P (list (vterm 'y)
                                                  (vterm 'z)
                                                  (vterm 'v)))

                            (literal t   'P (list (vterm 'x)
                                                  (vterm 'y)
                                                  (vterm 'u)))

                            (literal t   'P (list (vterm 'u)
                                                  (vterm 'z)
                                                  (vterm 'w)))))

              (clause (list (literal nil 'P (list (vterm 'u)
                                                  (vterm 'z)
                                                  (vterm 'w)))

                            (literal t   'P (list (vterm 'y)
                                                  (vterm 'z)
                                                  (vterm 'v)))

                            (literal t   'P (list (vterm 'x)
                                                  (vterm 'y)
                                                  (vterm 'u)))

                            (literal t   'P (list (vterm 'x)
                                                  (vterm 'v)
                                                  (vterm 'w)))))))


(clause-set (list

              (clause (list 
                        (literal t 'F (list (fterm 's (list (fterm 's (list (fterm 's (list (constant 'Z )))))))
                                            (fterm 's (list (fterm 's (list (fterm 's (list (fterm 's (list (fterm 's (list (fterm 's (list (constant 'Z ))))))) )))))))))
                      nil nil nil :conseq)

              (clause (list (literal nil 'P (list (constant 'Z )
                                                  (vterm 'x)
                                                  (vterm 'x)))))

              (clause (list (literal nil 'M (list (vterm 'x )
                                                  (constant 'Z )
                                                  (constant 'Z ))))) 

              (clause (list (literal nil 'F (list (constant 'Z )
                                                  (fterm 's (list (constant 'Z )))))))

              (clause (list (literal nil 'P (list (fterm 's (list (vterm 'x)))
                                                  (vterm 'y)
                                                  (fterm 's (list (vterm 'z)))))

                            (literal t   'P (list (vterm 'x)
                                                  (vterm 'y)
                                                  (vterm 'z)))))

              (clause (list (literal nil 'M (list (vterm 'm)
                                                  (fterm 's (list (vterm 'n)))
                                                  (vterm 'p)))
                            (literal t   'M (list (vterm 'm)
                                                  (vterm 'n)
                                                  (vterm 'k)))
                            (literal t   'P (list (vterm 'k)
                                                  (vterm 'm)
                                                  (vterm 'p)))))

              (clause (list (literal nil 'F (list (fterm 's (list (vterm 'k)))
                                                  (vterm 'r)))
                            (literal t   'F (list (vterm 'k)
                                                  (vterm 'm)))
                            (literal t   'M (list (vterm 'm)
                                                  (fterm 's (list (vterm 'k)))
                                                  (vterm 'r)))))))


(clause-set (list  (clause (list (literal nil 'P (list (constant 'A )
                                                                   (constant 'B )
                                                                   (constant 'C )))))
                               (clause (list (literal t   'P (list (constant 'B )
                                                                   (constant 'A )
                                                                   (constant 'C ))))
                                       nil nil nil :conseq)
                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (constant 'E )
                                                                   (vterm 'x)))))
                               (clause (list (literal nil 'P (list (constant 'E )
                                                                   (vterm 'x)
                                                                   (vterm 'x)))))
                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (vterm 'x)
                                                                   (constant 'E )))))
                               (clause (list (literal nil 'P (list (vterm 'u)
                                                                   (vterm 'z)
                                                                   (vterm 'w)))
                                             (literal t   'P (list (vterm 'y)
                                                                   (vterm 'z)
                                                                   (vterm 'v)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'y)
                                                                   (vterm 'u)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'v)
                                                                   (vterm 'w)))))
                               (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (vterm 'v)
                                                                   (vterm 'w)))
                                             (literal t   'P (list (vterm 'y)
                                                                   (vterm 'z)
                                                                   (vterm 'v)))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'y)
                                                                   (vterm 'u)))
                                             (literal t   'P (list (vterm 'u)
                                                                   (vterm 'z)
                                                                   (vterm 'w)))))))
))


(defparameter *prepare-resolution-test-data* (list
; snl
(clause-set (list  (clause (list (literal t 'P nil))
                           nil nil nil :conseq)
                                  (clause (list (literal nil 'P nil)))))
;default
(clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                   (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                   (clause (list (literal t 'P nil))
                           nil nil nil :conseq
                           )))
;snl
(clause-set (list  (clause (list (literal nil 'P nil)))
                                  (clause (list (literal t 'P nil) (literal nil 'Q nil)))
                                  (clause (list (literal t 'Q nil))
                                          nil nil nil :conseq)))

;snl
(clause-set (list  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'x)))))
                                  (clause (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                                (literal t 'R (list (vterm 'y) (vterm 'x)))
                                                (literal nil 'P (list (vterm 'y)))))
                                  (clause (list (literal nil 'R (list (constant 'A )
                                                                      (constant 'B )))))
                                  (clause (list (literal nil 'R (list (constant 'B )
                                                                      (constant 'A )))))
                                  (clause (list (literal t 'P (list (vterm 'x))))
                                          nil nil nil :conseq)))
;snl
(clause-set (list  (clause (list (literal nil 'LEN (list (constant 'NIL )
                                                                        (constant 'ZERO )))))
                                  (clause (list (literal t 'LEN (list (vterm 'c)
                                                                      (vterm 'n)))
                                                (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                                           (vterm 'c)))
                                                                        (fterm 'SUCC (list (vterm 'n)))))))
                                  (clause (list (literal t 'LEN (list (fterm 'CONS (list (constant 'A) 
                                                                                         (fterm 'CONS (list (constant 'B) (constant 'NIL)))))
                                                                      (vterm 'x))))
                                          nil nil nil :conseq)))
;default
(clause-set (list  (clause (list (literal nil 'P nil)))
                                  (clause (list (literal t 'P nil) (literal nil 'Q nil) (literal nil 'R nil)))
                                  (clause (list (literal t 'Q nil))
                                          nil nil nil :conseq)))
;snl
(clause-set (list              (clause (list (literal nil 'P (list (constant 'Z )
                                                                   (vterm 'x)
                                                                   (vterm 'x)))))

                               (clause (list (literal nil 'M (list (vterm 'x )
                                                                   (constant 'Z )
                                                                   (constant 'Z ))))) 

                               (clause (list (literal nil 'F (list (constant 'Z )
                                                                   (fterm 's (list (constant 'Z )))))))

                               (clause (list (literal t 'F (list (fterm 's (list (fterm 's (list (fterm 's (list (constant 'Z )))))))
			                                         (fterm 's (list (fterm 's (list (fterm 's (list (fterm 's (list (fterm 's (list (fterm 's (list (constant 'Z ))))))) )))))))))
                                       nil nil nil :conseq)


                               (clause (list (literal nil 'P (list (fterm 's (list (vterm 'x)))
                                                                   (vterm 'y)
                                                                   (fterm 's (list (vterm 'z)))))
                                             (literal t   'P (list (vterm 'x)
                                                                   (vterm 'y)
                                                                   (vterm 'z)))))

			        (clause (list (literal nil 'M (list (vterm 'm)
				                                    (fterm 's (list (vterm 'n)))
                                                                    (vterm 'p)))
                                              (literal t   'M (list (vterm 'm)
                                                                    (vterm 'n)
                                                                    (vterm 'k)))
					      (literal t   'P (list (vterm 'k)
                                                                    (vterm 'm)
                                                                    (vterm 'p)))))

				(clause (list (literal nil 'F (list (fterm 's (list (vterm 'k)))
                                                                    (vterm 'r)))
                                              (literal t   'F (list (vterm 'k)
                                                                    (vterm 'm)))
					      (literal t   'M (list (vterm 'm)
                                                                    (fterm 's (list (vterm 'k)))
                                                                    (vterm 'r)))))))
;default
(clause-set (list  (clause (list (literal t 'member (list (vterm 'x) (constant 'NIL)))))
                   (clause (list (literal nil 'member (list (vterm 'x) (fterm 'cons (list (vterm 'x) (vterm 'y)))))))
                   (clause (list (literal t 'member (list (vterm 'x) 
                                                          (vterm 'y)))
                                 (literal nil 'member (list (vterm 'x) 
                                                            (fterm 'cons (list (vterm 'z) (vterm 'y)))))
                                 ))
                   (clause (list (literal t 'member (list (constant 'A)
                                                          (fterm 'cons (list (constant 'C)
                                                                             (fterm 'cons (list (constant 'B)
                                                                                                (fterm 'cons (list (constant 'A) (constant 'NIL))))))))))
                           nil nil nil :conseq
                           )))
))


(test clover.tests.clover.simple
      (setf *save-resolution-history* nil)
      (loop :for target :in *simple-test-clause-set*
            :do
            (progn
              (format t "~%default~%")
              (multiple-value-bind
                  (foundp value) 
                  (time (clover.clover:start_resolution target))
                (is foundp))
              )))

(test clover.tests.clover.difficult
      (setf *save-resolution-history* nil)
      (loop :for target :in *difficult-test-clause-set*
            :do
            (progn
              (format t "~%default~%")
              (multiple-value-bind
                  (foundp value) 
                  (time (clover.clover:start_resolution target))
                (is foundp)))))

(test clover.tests.clover.prepare-resolution
      (loop :for target :in *prepare-resolution-test-data*
            :for expected :in (list :snl :default :snl :snl :snl :default :snl :default)
            :for converted := (clover.clover::prepare-resolution target)
            :do 
            (is (eq expected (clause-set.resolution-mode converted)))))

