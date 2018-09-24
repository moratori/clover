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
                                     (clause (list (literal t 'P nil))))))

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
                                                                         (vterm 'x)))) nil nil nil nil :center))))

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
                                     (clause (list (literal t 'P (list (vterm 'x))))))))
                
              (render-refutation-tree clause (merge-pathnames #P"test-output-files/test3.dot" 
                                                              (asdf:system-source-directory :clover)))
              t
              )))

        (is (progn
              (setf *save-resolution-history* t)
              (setf *resolution-algorithm* :linear)
              (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set (list  (clause (list (literal nil 'parent (list (fterm 'NAMIHEI nil)
                                                                              (fterm 'SAZAE nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'NAMIHEI nil)
                                                                              (fterm 'KATUO nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'NAMIHEI nil)
                                                                              (fterm 'WAKAME nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'FUNE nil)
                                                                              (fterm 'SAZAE nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'FUNE nil)
                                                                              (fterm 'KATUO nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'FUNE nil)
                                                                              (fterm 'WAKAME nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'SAZAE nil)
                                                                              (fterm 'TARA nil)))))
                                     (clause (list (literal nil 'parent (list (fterm 'MASUO nil)
                                                                              (fterm 'TARA nil)))))
                                     (clause (list (literal nil 'male (list (fterm 'MASUO nil)))))
                                     (clause (list (literal nil 'male (list (fterm 'TARA nil)))))
                                     (clause (list (literal nil 'male (list (fterm 'KATUO nil)))))
                                     (clause (list (literal nil 'male (list (fterm 'NAMIHEI nil)))))
                                     (clause (list (literal nil 'female (list (fterm 'FUNE nil)))))
                                     (clause (list (literal nil 'female (list (fterm 'SAZAE nil)))))
                                     (clause (list (literal nil 'female (list (fterm 'WAKAME nil)))))
                                     (clause (list (literal t 'parent (list (vterm 'x) (vterm 'y)))
                                                   (literal t 'male (list (vterm 'x)))
                                                   (literal nil 'father (list (vterm 'x) (vterm 'y)))))
                                     (clause (list (literal t 'parent (list (vterm 'x) (vterm 'y)))
                                                   (literal t 'female (list (vterm 'x)))
                                                   (literal nil 'mother (list (vterm 'x) (vterm 'y)))))
                                     (clause (list (literal t 'parent (list (vterm 'x) (vterm 'y)))
                                                   (literal nil 'ancestor (list (vterm 'x) (vterm 'y)))))
                                     (clause (list (literal t 'parent (list (vterm 'x) (vterm 'y)))
                                                   (literal t 'ancestor (list (vterm 'y) (vterm 'z)))
                                                   (literal nil 'ancestor (list (vterm 'x) (vterm 'z)))))
                                     (clause (list (literal t 'parent (list (vterm 'x) (vterm 'y)))
                                                   (literal t 'ancestor (list (vterm 'y) (vterm 'z)))
                                                   (literal nil 'ancestor (list (vterm 'x) (vterm 'z)))))
                                     (clause (list (literal t 'parent (list (vterm 'x) (vterm 'y)))
                                                   (literal t 'parent (list (vterm 'x) (vterm 'z)))
                                                   (literal nil 'sibling (list (vterm 'y) (vterm 'z)))))
                                     (clause (list (literal t 'sibling (list (vterm 'x) (vterm 'y)))
                                                   (literal t 'parent (list (vterm 'x) (vterm 'z)))
                                                   (literal t 'male (list (vterm 'z)))
                                                   (literal nil 'nephew (list (vterm 'z) (vterm 'y)))))
                                     (clause (list (literal t 'nephew (list (fterm 'TARA nil) (vterm 'x)))) nil nil nil nil :center))))

              (render-refutation-tree clause (merge-pathnames #P"test-output-files/test4.dot" 
                                                              (asdf:system-source-directory :clover)))
              t
              )))



        )
