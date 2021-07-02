(defpackage clover.tests.resolution
  (:use :cl
        :clover.types
        :clover.util
        :clover.unify
        :clover.rewrite
        :1am))
(in-package :clover.tests.resolution)


(test clover.tests.rewrite.rewrite-final.test1

      (let* ((target
              (fterm 'f (list (fterm 'f (list (fterm 'A nil) (vterm 'y)))
                              (fterm 'B nil))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'f (list (vterm 'x) (vterm 'y)))
                                (vterm 'x)))))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'A nil)))

        (is (term= result expected)))

      (let* ((target
               (fterm 'reverse (list (fterm 'cons (list (fterm 'A nil)
                                                        (fterm 'cons (list (fterm 'B nil)
                                                                           (fterm 'NIL nil)))))))
              )
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'append (list (fterm 'NIL nil) (vterm 'x)))
                                (vterm 'x))
                  (rewrite-rule (fterm 'append (list (fterm 'cons
                                                            (list (vterm 'x)
                                                                  (vterm 'y)))
                                                     (vterm 'z)))
                                (fterm 'cons (list (vterm 'x)
                                                   (fterm 'append (list (vterm 'y)
                                                                        (vterm 'z))))))
                  (rewrite-rule (fterm 'reverse (list (fterm 'NIL nil) ))
                                (fterm 'NIL nil))

                  (rewrite-rule (fterm 'reverse (list (fterm 'reverse (list
                                                                        (vterm 'x))) ))
                                (vterm 'x))
                  (rewrite-rule (fterm 'reverse (list (fterm 'cons (list (vterm 'x)
                                                                         (vterm 'y))) ))
                                (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                                     (fterm 'cons (list (vterm 'x)
                                                                        (fterm 'NIL nil))))))
                  (rewrite-rule (fterm 'reverse (list (fterm 'append (list (vterm 'x)
                                                                           (fterm 'cons (list (vterm 'y)
                                                                                              (fterm 'NIL nil)))))))
                                (fterm 'cons (list (vterm 'y)
                                                   (fterm 'reverse (list (vterm 'x))))))
                  )))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'cons (list (fterm 'B nil)
                                 (fterm 'cons (list (fterm 'A nil)
                                                    (fterm 'NIL nil)))))))

        (is (term= result expected)))
      )


(test clover.tests.rewrite.rewrite-final.test2

      (let* ((target
               (fterm 'reverse (list (vterm 'x))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'reverse (list (fterm 'NIL nil) ))
                                (fterm 'NIL nil))
                  )))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'reverse (list (vterm 'x)))))

        (is (term= result expected)))
      )

(test clover.tests.rewrite.rewrite-final.test3

      (let* ((target
               (fterm 'reverse (list (fterm 'NIL nil))))
             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'reverse (list (fterm 'NIL nil) ))
                                (fterm 'NIL nil))
                  )))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'NIL nil)))

        (is (term= result expected)))
      )

(test clover.tests.rewrite.rewrite-final.test4

      (let* ((target
               (fterm 'Inv (list
                             (fterm 'Inv (list 
                                           (fterm 'Inv (list 
                                                         (fterm 'Inv (list (fterm 'mult (list (vterm 'x)
                                                                                              (fterm 'Inv (list (vterm 'x))))))))))))))

             (rule-set
              (rewrite-rule-set
                (list
                  (rewrite-rule (fterm 'Inv (list (fterm 'E nil)))
                                (fterm 'E nil))
                  (rewrite-rule (fterm 'mult (list (fterm 'E nil) (vterm 'x)))
                                (vterm 'x))
                  (rewrite-rule (fterm 'mult (list (vterm 'x) (fterm 'E nil)))
                                (vterm 'x))
                  (rewrite-rule (fterm 'Inv (list (fterm 'Inv (list (vterm 'x)))))
                                (vterm 'x))
                  (rewrite-rule (fterm 'mult (list (fterm 'Inv (list (vterm 'x)))
                                                   (vterm 'x)
                                                   ))
                                (fterm 'E nil))
                  (rewrite-rule (fterm 'mult (list (vterm 'x) 
                                                   (fterm 'Inv (list (vterm 'x)))))
                                (fterm 'E nil))
                  (rewrite-rule (fterm 'Inv (list (fterm 'mult (list (vterm 'x)
                                                                     (vterm 'y)))))
                                (fterm 'mult (list (fterm 'Inv (list (vterm 'y)))
                                                   (fterm 'Inv (list (vterm 'x))))))
                  (rewrite-rule (fterm 'mult (list (fterm 'mult (list (vterm 'x)
                                                                      (vterm 'y)))
                                                   (vterm 'z)))
                                (fterm 'mult (list (vterm 'x)
                                                   (fterm 'mult (list (vterm 'y)
                                                                      (vterm 'z))))))
                  (rewrite-rule (fterm 'mult (list (fterm 'Inv (list (vterm 'x)))
                                                   (fterm 'mult (list (vterm 'x)
                                                                      (vterm 'y)))))
                                (vterm 'y))
                  (rewrite-rule (fterm 'mult (list (vterm 'x)
                                                   (fterm 'mult (list (fterm 'Inv (list (vterm 'x)))
                                                                      (vterm 'y)))))
                                (vterm 'y)))))
            (result
              (rewrite-final target rule-set))
            (expected
              (fterm 'E nil)))

        (is (term= result expected)))
      )

