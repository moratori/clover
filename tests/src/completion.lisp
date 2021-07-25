(defpackage clover.tests.completion
  (:use :cl
        :clover.types
        :clover.util
        :clover.completion
        :clover.rename
        :1am)
  (:import-from :clover.property
                :*term-order-algorithm*)
  )
(in-package :clover.tests.completion)



(test clover.tests.completion.collect-small-rules.test1
      (let* ((rule1
               (rewrite-rule
                 (fterm 'f (list (vterm 'x) (vterm 'y)))
                 (vterm 'x)))
             (rule2
               (rewrite-rule
                 (fterm 'f (list (vterm 'x) (vterm 'y)))
                 (vterm 'y)))
             (whole
               (rewrite-rule-set
                 (list rule1 rule2)))
             (target
               rule1))
        (is (null 
              (rewrite-rule-set.rewrite-rules 
                (clover.completion::%collect-small-rules rule1 whole))))
        (is (null 
              (rewrite-rule-set.rewrite-rules 
                (clover.completion::%collect-small-rules rule2 whole))))))

(test clover.tests.completion.collapse-rule.test1
      (let ((initial-e
              (equation-set nil))
            (initial-r
              (rewrite-rule-set
                (list
                  (rewrite-rule
                    (fterm 'f (list (vterm 'x) (vterm 'y)))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'f (list (vterm 'u) (vterm 'v)))
                    (vterm 'v))))))
        (multiple-value-bind (result-e result-r)
            (clover.completion::infer :collapse initial-e initial-r)
          (is (and (equation-set= initial-e result-e)
                   (rewrite-rule-set= initial-r result-r))))))

(test clover.tests.completion.collapse-rule.test2
      (let ((initial-e
              (equation-set nil))
            (initial-r
              (rewrite-rule-set
                (list
                  (rewrite-rule
                    (fterm 'f (list (vterm 'x) (vterm 'x)))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'f (list (vterm 'u) (vterm 'v)))
                    (vterm 'u))))))
        (multiple-value-bind (result-e result-r)
            (clover.completion::infer :collapse initial-e initial-r)
          (is (and (equation-set= 
                     result-e
                     (equation-set nil))
                   (rewrite-rule-set= 
                     result-r
                     (rewrite-rule-set
                       (list
                         (rewrite-rule
                           (fterm 'f (list (vterm 'u) (vterm 'v)))
                           (vterm 'u))))))))))

(test clover.tests.completion.deduce-rule.test1
      (let ((initial-e
              (equation-set nil))
            (initial-r
              (rewrite-rule-set
                 (list
                   (rewrite-rule
                     (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'x)))
                     (constant 'ZERO))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) (vterm 'z)))
                     (fterm 'plus (list (vterm 'x) (fterm 'plus (list (vterm 'y) (vterm 'z))))))
                   (rewrite-rule
                     (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (fterm 'plus (list (vterm 'x) (vterm 'y)))))
                     (vterm 'y))
                   (rewrite-rule
                     (fterm 'plus (list (vterm 'x) (constant 'ZERO)))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'inv (list (fterm 'inv (list (vterm 'x)))))
                     (vterm 'x))
                   (rewrite-rule
                     (fterm 'plus (list (vterm 'x) (fterm 'inv (list (vterm 'x)))))
                     (constant 'ZERO))
                   (rewrite-rule
                     (fterm 'inv (list (constant 'ZERO)))
                     (constant 'ZERO))
                   (rewrite-rule
                     (fterm 'plus (list (vterm 'x) (fterm 'plus (list (fterm 'inv (list (vterm 'x))) (vterm 'y)))))
                     (vterm 'y))
                   (rewrite-rule
                     (fterm 'inv (list (fterm 'plus (list (vterm 'x) (vterm 'y)))))
                     (fterm 'plus (list (fterm 'inv (list (vterm 'y)))
                                        (fterm 'inv (list (vterm 'x))))))))))
        (multiple-value-bind (result-e result-r)
            (clover.completion::infer :deduce initial-e initial-r)
          
          (is (and (equation-set= 
                     result-e
                     (equation-set nil))
                   (rewrite-rule-set=  result-r initial-r)))))
      )

(test clover.tests.completion.deduce-rule.test2
      (let ((initial-e
              (equation-set nil))
            (initial-r
              (rewrite-rule-set
                (list
                  (rewrite-rule
                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
                    (vterm 'x))
                  (rewrite-rule
                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
                                                            (vterm 'y)))
                                         (vterm 'z)))
                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
                  (rewrite-rule
                    (fterm 'reverse (list (constant 'NIL)))
                    (constant 'NIL))
                  (rewrite-rule
                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
                    (vterm 'x)
                    )
                  (rewrite-rule
                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                         (fterm 'cons (list (vterm 'x) (constant 'NIL))))))
                  (rewrite-rule
                    (fterm 'reverse (list (fterm 'append (list (vterm 'x) (fterm 'cons (list (vterm 'y) (constant 'NIL)))))))
                    (fterm 'cons (list (vterm 'y) (fterm 'reverse (list (vterm 'x))))))))))
        (multiple-value-bind (result-e result-r)
            (clover.completion::infer :deduce initial-e initial-r)
          
          (is (and (equation-set= 
                     result-e
                     (equation-set nil))
                   (rewrite-rule-set=  result-r initial-r))))))

(test clover.tests.completion.kb-completion.test1
      (setf *term-order-algorithm* :original)

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'h (list (fterm 'h (list (vterm 'x)))))
                    (fterm 'g (list (vterm 'x)))))))
            (result
              (rename-for-human-readable-printing
                (kb-completion target 10)))
            (expected
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (vterm 'CLOVER.PARSER::X))))
                  (rewrite-rule
                    (fterm 'h (list (fterm 'g (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    )
                  ))))

        (is (rewrite-rule-set= result expected))))


;(test clover.tests.completion.kb-completion.test2
;      (setf *term-order-algorithm* :original)
;      (setf CLOVER.COMPLETION::*debug-print* 0.3)
;
;      (let* ((target 
;              (equation-set
;                (list
;                  (equation 
;                    nil
;                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
;                    (vterm 'x))
;                  (equation
;                    nil
;                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
;                                                            (vterm 'y)))
;                                         (vterm 'z)))
;                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
;                  (equation
;                    nil
;                    (fterm 'reverse (list (constant 'NIL)))
;                    (constant 'NIL))
;                  (equation
;                    nil
;                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
;                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
;                                         (fterm 'cons (list (vterm 'x) (constant 'NIL)))))
;                    )
;                  (equation
;                    nil
;                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
;                    (vterm 'x)))))
;             (tmp 
;               (kb-completion target 10))
;             (result
;               (when tmp
;                 (rename-for-human-readable-printing tmp))))
;
;        (is (and result
;                 (not (null (rewrite-rule-set.rewrite-rules result)))))))

;(test clover.tests.completion.kb-completion.test3
;      (setf *term-order-algorithm* :original)
;      (setf CLOVER.COMPLETION::*debug-print* 0.1)
;
;      (let* ((target 
;              (equation-set
;                (list
;                  (equation 
;                    nil
;                    (vterm 'x)
;                    (fterm 'plus (list (constant 'ZERO) (vterm 'x))))
;                  (equation
;                    nil
;                    (constant 'ZERO)
;                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
;                                       (vterm 'x))))
;                  (equation 
;                    nil
;                    (fterm 'plus (list (vterm 'x)
;                                       (fterm 'plus (list (vterm 'y) (vterm 'z)))))
;                    (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
;                                       (vterm 'z)))))))
;            (result
;              (kb-completion target 100)))
;        (when result
;          (print (rename-for-human-readable-printing result)))
;        (is (and result (not (null (rewrite-rule-set.rewrite-rules result)))))))
