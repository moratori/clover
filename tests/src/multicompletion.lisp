(defpackage clover.tests.multicompletion
  (:use :cl
        :clover.types
        :clover.util
        :clover.multicompletion
        :clover.rename
        :1am)
  (:import-from :clover.parser
                :parse-mkbtt-expression)
  )
(in-package :clover.tests.multicompletion)


(defun symbol-set-equal (list1 list2)
  (and
    (null (set-difference list1 list2 :test #'eq))
    (null (set-difference list2 list1 :test #'eq))))


(test clover.tests.multicompletion.collect-constant-symbol.test1
      (is (symbol-set-equal
            (clover.multicompletion::collect-constant-symbol
              (fterm 'f (list (constant 'zero) (fterm 'g (list (constant 'one))))))
            (list 'zero 'one)))
      (is (symbol-set-equal
            (clover.multicompletion::collect-constant-symbol
              (constant 'zero))
            (list 'zero)))
      (is (symbol-set-equal
            (clover.multicompletion::collect-constant-symbol
              (vterm 'x))
            nil))
      (is (symbol-set-equal
            (clover.multicompletion::collect-constant-symbol
              (fterm 'f (list (vterm 'x))))
            nil))
      (is (symbol-set-equal
            (clover.multicompletion::collect-constant-symbol
              (fterm 'f (list (vterm 'x) (constant 'ZERO) (constant 'ZERO))))
            (list 'zero 'zero)))
      )

(test clover.tests.multicompletion.collect-function-symbol.test1
      (is (symbol-set-equal
            (clover.multicompletion::collect-function-symbol
              (fterm 'f (list (constant 'zero) (fterm 'g (list (constant 'one))))))
            (list 'f 'g)))
      (is (symbol-set-equal
            (clover.multicompletion::collect-function-symbol
              (constant 'zero))
            nil))
      (is (symbol-set-equal
            (clover.multicompletion::collect-function-symbol
              (vterm 'x))
            nil))
      (is (symbol-set-equal
            (clover.multicompletion::collect-function-symbol
              (fterm 'f (list (vterm 'x))))
            (list 'f)))
      (is (symbol-set-equal
            (clover.multicompletion::collect-function-symbol
              (fterm 'f (list (vterm 'x) (constant 'ZERO) (constant 'ZERO))))
            (list 'f)))
      (is (symbol-set-equal
            (clover.multicompletion::collect-function-symbol
              (fterm 'f (list (fterm 'f (list (vterm 'x))))))
            (list 'f)))
      )


(test clover.tests.multicompletion.permutation.test1
      (is
        (let ((expected nil)
              (result (clover.multicompletion::take
                        300 
                        (clover.multicompletion::permutation nil))))
          (and
            (null (set-difference result expected :test #'equalp))
            (null (set-difference expected result :test #'equalp)))))

      (is
        (let ((expected (list (list 1)))
              (result (clover.multicompletion::take
                        300 
                        (clover.multicompletion::permutation
                          (list 1)))))
          (and
            (null (set-difference result expected :test #'equalp))
            (null (set-difference expected result :test #'equalp)))))

      (is
        (let ((expected (list (list 1 2)
                               (list 2 1)))
              (result (clover.multicompletion::take
                        300 
                        (clover.multicompletion::permutation
                          (list 1 2)))))
          (and
            (null (set-difference result expected :test #'equalp))
            (null (set-difference expected result :test #'equalp)))))

      (is
        (let ((expected (list (list 1 2 3)
                               (list 1 3 2)
                               (list 2 1 3)
                               (list 2 3 1)
                               (list 3 2 1)
                               (list 3 1 2)))
              (result (clover.multicompletion::take
                        300 
                        (clover.multicompletion::permutation
                          (list 1 2 3)))))
          (and
            (null (set-difference result expected :test #'equalp))
            (null (set-difference expected result :test #'equalp)))))

      (is
        (let ((expected (list (list nil)))
              (result (clover.multicompletion::take
                        300 
                        (clover.multicompletion::permutation
                          (list nil)))))
          (and
            (null (set-difference result expected :test #'equalp))
            (null (set-difference expected result :test #'equalp)))))

      (is
        (let ((expected (list (list nil t)
                               (list t nil)))
              (result (clover.multicompletion::take
                        300 
                        (clover.multicompletion::permutation
                          (list nil t)))))
          (and
            (null (set-difference result expected :test #'equalp))
            (null (set-difference expected result :test #'equalp))))))


(test clover.tests.multicompletion.multi-kb-completion.test1

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'h (list (fterm 'h (list (vterm 'x)))))
                    (fterm 'g (list (vterm 'x)))))))
            (result
              (rename-for-human-readable-printing
                (multiple-value-bind (_1 _2 completed)
                    (clover.multicompletion::multi-kb-completion target 10)
                  completed
                  )))
            (expected1 ;; g < hの場合
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (vterm 'CLOVER.PARSER::X))))
                  (rewrite-rule
                    (fterm 'h (list (fterm 'g (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    )
                  )))
            (expected2 ;; h < gの場合
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'g (list (vterm 'CLOVER.PARSER::x)))
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::x))))))))))
        (is 
          (or
            (rewrite-rule-set= result expected1)
            (rewrite-rule-set= result expected2)))))


(test clover.tests.multicompletion.multi-kb-completion.test2

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
                    (vterm 'x))
                  (equation
                    nil
                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
                                                            (vterm 'y)))
                                         (vterm 'z)))
                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
                  (equation
                    nil
                    (fterm 'reverse (list (constant 'NIL)))
                    (constant 'NIL))
                  (equation
                    nil
                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                         (fterm 'cons (list (vterm 'x) (constant 'NIL)))))
                    )
                  (equation
                    nil
                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
                    (vterm 'x)))))
             (tmp 
               (multiple-value-bind (_1 _2 completed)
                   (clover.multicompletion::multi-kb-completion target 10)
                 completed))
             (result
               (when tmp
                 (rename-for-human-readable-printing tmp)))
             (expected
               (rewrite-rule-set
                 (list
                   (rewrite-rule
                     (fterm 'append (list (constant 'NIL) (vterm 'CLOVER.PARSER::x)))
                     (vterm 'CLOVER.PARSER::x))
                   (rewrite-rule
                     (fterm 'reverse (list (constant 'NIL)))
                     (constant 'NIL))
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'reverse (list (vterm 'CLOVER.PARSER::x)))))
                     (vterm 'CLOVER.PARSER::x))
                   (rewrite-rule
                     (fterm 'append (list (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y))) (vterm 'CLOVER.PARSER::z)))
                     (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (fterm 'append (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::Z))))))
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'cons (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::x)))))
                     (fterm 'append (list (fterm 'reverse (list (vterm 'CLOVER.PARSER::x))) (fterm 'cons (list (vterm 'CLOVER.PARSER::y) (constant 'NIL)))))) 
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'append (list (vterm 'CLOVER.PARSER::y) (fterm 'cons (list (vterm 'CLOVER.PARSER::x)(constant 'NIL)))))))
                     (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (fterm 'reverse (list (vterm 'CLOVER.PARSER::y))))))))))
        (is (and result (rewrite-rule-set= result expected)))))

(test clover.tests.multicompletion.multi-kb-completion.test3

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (vterm 'x)
                    (fterm 'plus (list (constant 'ZERO) (vterm 'x))))
                  (equation
                    nil
                    (constant 'ZERO)
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x))))
                  (equation 
                    nil
                    (fterm 'plus (list (vterm 'x)
                                       (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                    (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                       (vterm 'z)))))))
            (result
              (rename-for-human-readable-printing
                (multiple-value-bind (_1 _2 completed)
                    (clover.multicompletion::multi-kb-completion target 100)
                  completed
                  )))
            (expected
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'plus (list (constant 'zero) (vterm 'CLOVER.PARSER::x)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::x)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (constant 'zero) (vterm 'CLOVER.PARSER::x)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::x)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y))) (vterm 'CLOVER.PARSER::z)))
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'plus (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::z))))))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y)))))
                    (vterm 'CLOVER.PARSER::y))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (constant 'zero)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x)))))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'inv (list (vterm 'CLOVER.PARSER::x)))))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'inv (list (constant 'zero)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::y)))))
                    (vterm 'CLOVER.PARSER::y))
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'plus (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::x)))))
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (fterm 'inv (list (vterm 'CLOVER.PARSER::y))))))))))
        (is (and result (rewrite-rule-set= result expected)))))



(test clover.tests.multicompletion.multi-kb-completion.test4
      ;; コマンドラインから実行したときに発生した例外が表示されるか確認する
      (is
        (clover.multicompletion::multi-kb-completion
          (parse-mkbtt-expression
"(VAR x y)
(RULES
cons(S, cons(W, x)) -> cons(W, x)
cons(x, cons(S, cons(W, y))) -> cons(x, cons(W, y))
cons(W, cons(B, x)) -> cons(S, x)
cons(x, cons(W, cons(B, y))) -> cons(x, cons(S, y))
)"
            )
          15)
        )
)










(test clover.tests.multicompletion.iterative-multi-kb-completion.test1

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'h (list (fterm 'h (list (vterm 'x)))))
                    (fterm 'g (list (vterm 'x)))))))
            (result
              (rename-for-human-readable-printing
                (multiple-value-bind (_1 _2 completed)
                    (iterative-multi-kb-completion target)
                  completed
                  )))
            (expected1 ;; g < hの場合
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (vterm 'CLOVER.PARSER::X))))
                  (rewrite-rule
                    (fterm 'h (list (fterm 'g (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    )
                  )))
            (expected2 ;; h < gの場合
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'g (list (vterm 'CLOVER.PARSER::x)))
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::x))))))))))
        (is 
          (or
            (rewrite-rule-set= result expected1)
            (rewrite-rule-set= result expected2)))))


(test clover.tests.multicompletion.iterative-multi-kb-completion.test2

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
                    (vterm 'x))
                  (equation
                    nil
                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
                                                            (vterm 'y)))
                                         (vterm 'z)))
                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
                  (equation
                    nil
                    (fterm 'reverse (list (constant 'NIL)))
                    (constant 'NIL))
                  (equation
                    nil
                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                         (fterm 'cons (list (vterm 'x) (constant 'NIL)))))
                    )
                  (equation
                    nil
                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
                    (vterm 'x)))))
             (tmp 
               (multiple-value-bind (_1 _2 completed)
                   (iterative-multi-kb-completion target)
                 completed))
             (result
               (when tmp
                 (rename-for-human-readable-printing tmp)))
             (expected
               (rewrite-rule-set
                 (list
                   (rewrite-rule
                     (fterm 'append (list (constant 'NIL) (vterm 'CLOVER.PARSER::x)))
                     (vterm 'CLOVER.PARSER::x))
                   (rewrite-rule
                     (fterm 'reverse (list (constant 'NIL)))
                     (constant 'NIL))
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'reverse (list (vterm 'CLOVER.PARSER::x)))))
                     (vterm 'CLOVER.PARSER::x))
                   (rewrite-rule
                     (fterm 'append (list (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y))) (vterm 'CLOVER.PARSER::z)))
                     (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (fterm 'append (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::Z))))))
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'cons (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::x)))))
                     (fterm 'append (list (fterm 'reverse (list (vterm 'CLOVER.PARSER::x))) (fterm 'cons (list (vterm 'CLOVER.PARSER::y) (constant 'NIL)))))) 
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'append (list (vterm 'CLOVER.PARSER::y) (fterm 'cons (list (vterm 'CLOVER.PARSER::x)(constant 'NIL)))))))
                     (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (fterm 'reverse (list (vterm 'CLOVER.PARSER::y))))))))))
        (is (and result (rewrite-rule-set= result expected)))))

(test clover.tests.multicompletion.iterative-multi-kb-completion.test3

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (vterm 'x)
                    (fterm 'plus (list (constant 'ZERO) (vterm 'x))))
                  (equation
                    nil
                    (constant 'ZERO)
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x))))
                  (equation 
                    nil
                    (fterm 'plus (list (vterm 'x)
                                       (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                    (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                       (vterm 'z)))))))
            (result
              (rename-for-human-readable-printing
                (multiple-value-bind (_1 _2 completed)
                    (iterative-multi-kb-completion target)
                  completed
                  )))
            (expected
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'plus (list (constant 'zero) (vterm 'CLOVER.PARSER::x)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::x)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (constant 'zero) (vterm 'CLOVER.PARSER::x)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::x)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y))) (vterm 'CLOVER.PARSER::z)))
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'plus (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::z))))))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y)))))
                    (vterm 'CLOVER.PARSER::y))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (constant 'zero)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x)))))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'inv (list (vterm 'CLOVER.PARSER::x)))))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'inv (list (constant 'zero)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::y)))))
                    (vterm 'CLOVER.PARSER::y))
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'plus (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::x)))))
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (fterm 'inv (list (vterm 'CLOVER.PARSER::y))))))))))
        (is (and result (rewrite-rule-set= result expected)))))


(test clover.tests.multicompletion.get-number-of-processors.test1
      (is (<= 1 (clover.multicompletion::get-number-of-processors)))
      (is (<= 1 (clover.multicompletion::get-number-of-processors)))
      )




(test clover.tests.multicompletion.toplevel-completion.test1

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'h (list (fterm 'h (list (vterm 'x)))))
                    (fterm 'g (list (vterm 'x)))))))
            (result
              (rename-for-human-readable-printing
                (multiple-value-bind (_1 _2 completed)
                    (toplevel-completion target)
                  completed
                  )))
            (expected1 ;; g < hの場合
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (vterm 'CLOVER.PARSER::X))))
                  (rewrite-rule
                    (fterm 'h (list (fterm 'g (list (vterm 'CLOVER.PARSER::X)))))
                    (fterm 'g (list (fterm 'h (list (vterm 'CLOVER.PARSER::X)))))
                    )
                  )))
            (expected2 ;; h < gの場合
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'g (list (vterm 'CLOVER.PARSER::x)))
                    (fterm 'h (list (fterm 'h (list (vterm 'CLOVER.PARSER::x))))))))))
        (is 
          (or
            (rewrite-rule-set= result expected1)
            (rewrite-rule-set= result expected2)))))


(test clover.tests.multicompletion.toplevel-completion.test2

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (fterm 'append (list (constant 'NIL) (vterm 'x)))
                    (vterm 'x))
                  (equation
                    nil
                    (fterm 'append (list (fterm 'cons (list (vterm 'x) 
                                                            (vterm 'y)))
                                         (vterm 'z)))
                    (fterm 'cons (list (vterm 'x) (fterm 'append (list (vterm 'y) (vterm 'z))))))
                  (equation
                    nil
                    (fterm 'reverse (list (constant 'NIL)))
                    (constant 'NIL))
                  (equation
                    nil
                    (fterm 'reverse (list (fterm 'cons (list (vterm 'x) (vterm 'y)))))
                    (fterm 'append (list (fterm 'reverse (list (vterm 'y)))
                                         (fterm 'cons (list (vterm 'x) (constant 'NIL)))))
                    )
                  (equation
                    nil
                    (fterm 'reverse (list (fterm 'reverse (list (vterm 'x)))))
                    (vterm 'x)))))
             (tmp 
               (multiple-value-bind (_1 _2 completed)
                   (toplevel-completion target)
                 completed))
             (result
               (when tmp
                 (rename-for-human-readable-printing tmp)))
             (expected
               (rewrite-rule-set
                 (list
                   (rewrite-rule
                     (fterm 'append (list (constant 'NIL) (vterm 'CLOVER.PARSER::x)))
                     (vterm 'CLOVER.PARSER::x))
                   (rewrite-rule
                     (fterm 'reverse (list (constant 'NIL)))
                     (constant 'NIL))
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'reverse (list (vterm 'CLOVER.PARSER::x)))))
                     (vterm 'CLOVER.PARSER::x))
                   (rewrite-rule
                     (fterm 'append (list (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y))) (vterm 'CLOVER.PARSER::z)))
                     (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (fterm 'append (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::Z))))))
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'cons (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::x)))))
                     (fterm 'append (list (fterm 'reverse (list (vterm 'CLOVER.PARSER::x))) (fterm 'cons (list (vterm 'CLOVER.PARSER::y) (constant 'NIL)))))) 
                   (rewrite-rule
                     (fterm 'reverse (list (fterm 'append (list (vterm 'CLOVER.PARSER::y) (fterm 'cons (list (vterm 'CLOVER.PARSER::x)(constant 'NIL)))))))
                     (fterm 'cons (list (vterm 'CLOVER.PARSER::x) (fterm 'reverse (list (vterm 'CLOVER.PARSER::y))))))))))
        (is (and result (rewrite-rule-set= result expected)))))

(test clover.tests.multicompletion.toplevel-completion.test3

      (let* ((target 
              (equation-set
                (list
                  (equation 
                    nil
                    (vterm 'x)
                    (fterm 'plus (list (constant 'ZERO) (vterm 'x))))
                  (equation
                    nil
                    (constant 'ZERO)
                    (fterm 'plus (list (fterm 'inv (list (vterm 'x)))
                                       (vterm 'x))))
                  (equation 
                    nil
                    (fterm 'plus (list (vterm 'x)
                                       (fterm 'plus (list (vterm 'y) (vterm 'z)))))
                    (fterm 'plus (list (fterm 'plus (list (vterm 'x) (vterm 'y))) 
                                       (vterm 'z)))))))
            (result
              (rename-for-human-readable-printing
                (multiple-value-bind (_1 _2 completed)
                    (toplevel-completion target)
                  completed
                  )))
            (expected
              (rewrite-rule-set
                (list 
                  (rewrite-rule
                    (fterm 'plus (list (constant 'zero) (vterm 'CLOVER.PARSER::x)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::x)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (constant 'zero) (vterm 'CLOVER.PARSER::x)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::x)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y))) (vterm 'CLOVER.PARSER::z)))
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'plus (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::z))))))
                  (rewrite-rule
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (vterm 'CLOVER.PARSER::y)))))
                    (vterm 'CLOVER.PARSER::y))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (constant 'zero)))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x)))))
                    (vterm 'CLOVER.PARSER::x))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'inv (list (vterm 'CLOVER.PARSER::x)))))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'inv (list (constant 'zero)))
                    (constant 'zero))
                  (rewrite-rule
                    (fterm 'plus (list (vterm 'CLOVER.PARSER::x) (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (vterm 'CLOVER.PARSER::y)))))
                    (vterm 'CLOVER.PARSER::y))
                  (rewrite-rule
                    (fterm 'inv (list (fterm 'plus (list (vterm 'CLOVER.PARSER::y) (vterm 'CLOVER.PARSER::x)))))
                    (fterm 'plus (list (fterm 'inv (list (vterm 'CLOVER.PARSER::x))) (fterm 'inv (list (vterm 'CLOVER.PARSER::y))))))))))
        (is (and result (rewrite-rule-set= result expected)))))


(test clover.tests.multicompletion.get-number-of-processors.test1
      (is (<= 1 (clover.multicompletion::get-number-of-processors)))
      (is (<= 1 (clover.multicompletion::get-number-of-processors)))
      )

