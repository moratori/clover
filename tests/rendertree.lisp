(defpackage clover.tests.rendertree
  (:use :cl
        :clover.property
        :clover.clover
        :clover.types
        :clover.util
        :clover.unify
        :clover.rendertree
        :1am))
(in-package :clover.tests.rendertree)




(test clover.tests.rendertree.render-refutation-tree

        (is (progn
              (setf *save-resolution-history* t)
              (setf *resolution-algorithm* :exhaustive)
              (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set (list  (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                     (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                                     (clause (list (literal t 'P nil)) nil nil nil nil :goal))))

              (render-refutation-tree clause (merge-pathnames #P"test-output-files/test1.dot" 
                                                              (asdf:system-source-directory :clover)))
              t
              )))
        
        (is (progn
              (setf *save-resolution-history* t)
              (setf *resolution-algorithm* :linear)
              (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set (list  (clause (list (literal nil 'LEN (list (fterm 'NIL nil)
                                                                           (fterm 'ZERO nil)))))
                                     (clause (list (literal t 'LEN (list (vterm 'c)
                                                                         (vterm 'n)))
                                                   (literal nil 'LEN (list (fterm 'CONS (list (vterm 'e)
                                                                                              (vterm 'c)))
                                                                           (fterm 'SUCC (list (vterm 'n)))))))
                                     (clause (list (literal t 'LEN (list (fterm 'CONS (list (fterm 'A nil) 
                                                                                            (fterm 'CONS (list (fterm 'B nil) (fterm 'NIL nil)))))
                                                                         (vterm 'x)))) nil nil nil nil :goal))))

              (render-refutation-tree clause (merge-pathnames #P"test-output-files/test2.dot" 
                                                              (asdf:system-source-directory :clover)))
              t
              )))
        
        (is (progn
              (setf *save-resolution-history* t)
              (setf *resolution-algorithm* :exhaustive)
              (multiple-value-bind (depth clause) 
                (start_resolution
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
                                     (clause (list (literal t 'P (list (vterm 'x)))) nil nil nil nil :goal))))
                
              (render-refutation-tree clause (merge-pathnames #P"test-output-files/test3.dot" 
                                                              (asdf:system-source-directory :clover)))
              t
              )))




        )
