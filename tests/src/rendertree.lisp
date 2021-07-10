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


(defparameter *graphviz-output-dir* (merge-pathnames 
                                      #P"tests/test-output-files/graphviz/"
                                      (asdf:system-source-directory :clover)))

(test clover.tests.rendertree.%check-renderable-to-terminal.test1
      (is (clover.rendertree::%check-renderable-to-terminal
            (clause (list (literal nil 'P nil) (literal nil 'P nil)))))
      (is (clover.rendertree::%check-renderable-to-terminal
            (clause (list (literal nil 'P nil) (literal nil 'P nil))
                    (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                    (clause (list (literal nil 'P nil) (literal nil 'P nil))))))
      (is (clover.rendertree::%check-renderable-to-terminal
            (clause (list (literal nil 'P nil) (literal nil 'P nil))
                    (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                    (clause (list (literal nil 'P nil) (literal nil 'P nil))
                            (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                            (clause (list (literal nil 'P nil) (literal nil 'P nil))
                                    (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                    (clause (list (literal nil 'P nil) (literal nil 'P nil))))))))
 )

(test clover.tests.rendertree.%check-renderable-to-terminal.test2
      (is (not (clover.rendertree::%check-renderable-to-terminal
                 (clause (list (literal nil 'P nil) (literal nil 'P nil))
                         (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                         (clause (list (literal nil 'P nil) (literal nil 'P nil))
                                 (clause (list (literal nil 'P nil) (literal nil 'P nil))
                                         (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                         (clause (list (literal nil 'P nil) (literal nil 'P nil))))
                                 (clause (list (literal nil 'P nil) (literal nil 'P nil))
                                         (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                         (clause (list (literal nil 'P nil) (literal nil 'P nil)))))))))
      )

(test clover.tests.rendertree.render-refutation-tree.test1
      (is (progn
            (setf *save-resolution-history* t)
            (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set (list (clause (list (literal nil 'P nil) (literal nil 'P nil)))
                                    (clause (list (literal nil 'P nil) (literal t 'Q nil)))
                                    (clause (list (literal t 'P nil))))))

              (render-refutation-tree 
                clause (merge-pathnames #P"test1.dot"  *graphviz-output-dir*))
              t))))

(test clover.tests.rendertree.render-refutation-tree.test2
      (is (progn
            (setf *save-resolution-history* t)
            (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set 
                    (list (clause 
                            (list (literal nil 'LEN (list (constant 'NIL )
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
                                                        (vterm 'x)))) nil nil nil))))

              (render-refutation-tree 
                clause (merge-pathnames #P"test2.dot"  *graphviz-output-dir*))
              t))))

(test clover.tests.rendertree.render-refutation-tree.test3
      (is (progn
            (setf *save-resolution-history* t)
            (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set 
                    (list (clause 
                            (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                  (literal t 'R (list (vterm 'y) (vterm 'x)))
                                  (literal nil 'P (list (vterm 'x)))))
                          (clause 
                            (list (literal t 'R (list (vterm 'x) (vterm 'y)))
                                  (literal t 'R (list (vterm 'y) (vterm 'x)))
                                  (literal nil 'P (list (vterm 'y)))))
                          (clause (list (literal nil 'R (list (constant 'A )
                                                              (constant 'B )))))
                          (clause (list (literal nil 'R (list (constant 'B )
                                                              (constant 'A )))))
                          (clause (list (literal t 'P (list (vterm 'x))))))))
                
              (render-refutation-tree 
                clause (merge-pathnames #P"test3.dot"  *graphviz-output-dir*))
              t))))

(test clover.tests.rendertree.render-refutation-tree.test4
      (is (progn
            (setf *save-resolution-history* t)
            (multiple-value-bind (depth clause) 
                (start_resolution
                  (clause-set 
                    (list  (clause (list (literal t 'nephew (list (constant 'TARA ) (vterm 'x)))) nil nil nil)
                           (clause 
                             (list (literal nil 'parent (list (constant 'NAMIHEI )
                                                              (constant 'SAZAE )))))
                           (clause (list (literal nil 'parent (list (constant 'NAMIHEI )
                                                                    (constant 'KATUO )))))
                           (clause (list (literal nil 'parent (list (constant 'NAMIHEI )
                                                                    (constant 'WAKAME )))))
                           (clause (list (literal nil 'parent (list (constant 'FUNE )
                                                                    (constant 'SAZAE )))))
                           (clause (list (literal nil 'parent (list (constant 'FUNE )
                                                                    (constant 'KATUO )))))
                           (clause (list (literal nil 'parent (list (constant 'FUNE )
                                                                    (constant 'WAKAME )))))
                           (clause (list (literal nil 'parent (list (constant 'SAZAE )
                                                                    (constant 'TARA )))))
                           (clause (list (literal nil 'parent (list (constant 'MASUO )
                                                                    (constant 'TARA )))))
                           (clause (list (literal nil 'male (list (constant 'MASUO )))))
                           (clause (list (literal nil 'male (list (constant 'TARA )))))
                           (clause (list (literal nil 'male (list (constant 'KATUO )))))
                           (clause (list (literal nil 'male (list (constant 'NAMIHEI )))))
                           (clause (list (literal nil 'female (list (constant 'FUNE )))))
                           (clause (list (literal nil 'female (list (constant 'SAZAE )))))
                           (clause (list (literal nil 'female (list (constant 'WAKAME )))))
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
                                         (literal nil 'nephew (list (vterm 'z) (vterm 'y))))))))

              (render-refutation-tree 
                clause (merge-pathnames #P"test4.dot"  *graphviz-output-dir*))
              t))))
