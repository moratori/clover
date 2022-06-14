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


(test clover.tests.clover.start_resolution.test1
      (let ((linear-sucess
             (list 
               (clause-set (list  (clause (list (literal t 'P nil)))
                                  (clause (list (literal nil 'P nil)))))
               (clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                  (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                                  (clause (list (literal t 'P nil)))))
               (clause-set (list  (clause (list (literal nil 'P nil)))
                                  (clause (list (literal t 'P nil) (literal nil 'Q nil)))
                                  (clause (list (literal t 'Q nil)))))
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
                                  (clause (list (literal t 'P (list (vterm 'x)))))))
               (clause-set (list  (clause (list (literal nil 'LEN (list (constant 'NIL )
                                                                        (constant 'ZERO )))))
                                  (clause (list (literal t 'LEN (list (vterm 'c)
                                                                      (vterm 'n)))
                                                (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                                           (vterm 'c)))
                                                                        (fterm 'SUCC (list (vterm 'n)))))))
                                  (clause (list (literal t 'LEN (list (fterm 'CONS (list (constant 'A) 
                                                                                         (fterm 'CONS (list (constant 'B) (constant 'NIL)))))
                                                                      (vterm 'x))))))))))
  (setf *save-resolution-history* nil)
  (loop :for each :in linear-sucess
        :do (is (start_resolution each)))))




(test clover.tests.clover.start_resolution.test2
      (setf *save-resolution-history* nil)
      (is (start_resolution
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
                                                                      (constant 'C ))))))))))

(test clover.tests.clover.start_resolution.test3
      (setf *save-resolution-history* nil)
      (is (start_resolution
            (clause-set (list (clause (list (literal t   'P (list (constant 'B )
                                                                  (constant 'A )
                                                                  (constant 'C )))))
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
                              ))))
      )


(test clover.tests.clover.start_resolution.test4
      (setf *save-resolution-history* nil)
      (is (start_resolution
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
                                                        (vterm 'x)))) nil nil nil))))))


(test clover.tests.clover.start_resolution.test5
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (cnt value) (start_resolution
            (clause-set (list  (clause (list (literal t 'P (list (constant 'C )
                                                                 (constant 'B )
                                                                 (constant 'A ))))) 
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
                                                                   (vterm 'w))))))))
        (is (> cnt 0))))

(test clover.tests.clover.start_resolution.test6
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (cnt value) (start_resolution
            (clause-set (list  (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (constant 'E )
                                                                   (vterm 'x)))))
                               (clause (list (literal t 'P (list (constant 'B )
                                                                 (constant 'A )
                                                                 (constant 'C ))))) 
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
                                                                   (vterm 'w))))))))
        (is (> cnt 0))))


(test clover.tests.clover.start_resolution.test7
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (cnt value) (start_resolution
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
                                                                 (constant 'B )))))

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
                                                                   (vterm 'w))))))))
        (is (> cnt 0))))



(test clover.tests.clover.astar-resolution.test1
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (foundp value) (clover.clover::astar-resolution
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
                                                                 (constant 'B )))))

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
                                                                   (vterm 'w))))))))
        (is foundp)))


(test clover.tests.clover.astar-resolution.test2
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (foundp value) (clover.clover::astar-resolution
            (clause-set (list  (clause (list (literal nil 'P (list (vterm 'x)
                                                                   (constant 'E )
                                                                   (vterm 'x)))))
                               (clause (list (literal t 'P (list (constant 'B )
                                                                 (constant 'A )
                                                                 (constant 'C ))))) 
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
                                                                   (vterm 'w))))))))
        (is foundp)))


(test clover.tests.clover.astar-resolution.test3
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (foundp value) (clover.clover::astar-resolution
            (clause-set (list  (clause (list (literal t 'P (list (constant 'C )
                                                                 (constant 'B )
                                                                 (constant 'A ))))) 
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
                                                                   (vterm 'w))))))))
        (is foundp)))

(test clover.tests.clover.astar-resolution.test4
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (foundp value) (clover.clover::astar-resolution
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
                                                        (vterm 'x)))) nil nil nil)))
            )
        (is foundp)))


(test clover.tests.clover.astar-resolution.test5
      (setf *save-resolution-history* nil)
      (multiple-value-bind
          (foundp value) (clover.clover::astar-resolution
                           (clause-set (list (clause (list (literal t   'P (list (constant 'B )
                                                                  (constant 'A )
                                                                  (constant 'C )))))
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
                              )))
        (is foundp)))


;(test clover.tests.clover.astar-resolution.test6
;      (setf *save-resolution-history* nil)
;      (multiple-value-bind
;          (cnt value) (clover.clover::astar-resolution
;            (clause-set (list  (clause (list (literal nil 'P (list (constant 'A )
;                                                                   (constant 'B )
;                                                                   (constant 'C )))))
;                               (clause (list (literal t   'P (list (constant 'B )
;                                                                   (constant 'A )
;                                                                   (constant 'C )))))
;                               (clause (list (literal nil 'P (list (vterm 'x)
;                                                                   (constant 'E )
;                                                                   (vterm 'x)))))
;                               (clause (list (literal nil 'P (list (constant 'E )
;                                                                   (vterm 'x)
;                                                                   (vterm 'x)))))
;                               (clause (list (literal nil 'P (list (vterm 'x)
;                                                                   (vterm 'x)
;                                                                   (constant 'E )))))
;                               (clause (list (literal nil 'P (list (vterm 'u)
;                                                                   (vterm 'z)
;                                                                   (vterm 'w)))
;                                             (literal t   'P (list (vterm 'y)
;                                                                   (vterm 'z)
;                                                                   (vterm 'v)))
;                                             (literal t   'P (list (vterm 'x)
;                                                                   (vterm 'y)
;                                                                   (vterm 'u)))
;                                             (literal t   'P (list (vterm 'x)
;                                                                   (vterm 'v)
;                                                                   (vterm 'w)))))
;                               (clause (list (literal nil 'P (list (vterm 'x)
;                                                                   (vterm 'v)
;                                                                   (vterm 'w)))
;                                             (literal t   'P (list (vterm 'y)
;                                                                   (vterm 'z)
;                                                                   (vterm 'v)))
;                                             (literal t   'P (list (vterm 'x)
;                                                                   (vterm 'y)
;                                                                   (vterm 'u)))
;                                             (literal t   'P (list (vterm 'u)
;                                                                   (vterm 'z)
;                                                                   (vterm 'w))))))))
;        (is cnt)))

