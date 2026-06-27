(defpackage clover.tests.unify
  (:use :cl
        :clover.conditions
        :clover.unify
        :clover.types
        :clover.util
        :1am))
(in-package :clover.tests.unify)



(test clover.tests.unify.%collect-disagreement-set 
      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (vterm 'x) (vterm 'y))))
          (unifier-set= 
            us
            (unifier-set 
              (list (unifier (vterm 'x) (vterm 'y)))))))

      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (vterm 'x) (fterm 'f (list (vterm 'z))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'f (list (vterm 'z)))))))))

      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (fterm 'f (list (vterm 'x))) (fterm 'f (list (vterm 'z))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'z)))))))
      
      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
                  (fterm 'f (list (vterm 'x))) (fterm 'f (list (fterm 'g (list (vterm 'z))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'g (list (vterm 'z)))))))))
      
      (is 
        (let ((us 
                (clover.unify::%collect-disagreement-set 
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
            (clover.unify::%collect-disagreement-set 
              (vterm 'x) (fterm 'f (list (vterm 'x))))
            (occurrence-check-error (e)
              t)))
      
      (is 
        (handler-case
            (clover.unify::%collect-disagreement-set 
              (fterm 'f (list (vterm 'x))) (fterm 'f (list (vterm 'x))))
            (occurrence-check-error (e)
              t)))

      (is 
        (handler-case
            (clover.unify::%collect-disagreement-set 
              (fterm 'g (list (vterm 'x))) (fterm 'f (list (vterm 'z))))
            (unmatching-fterm-error (e)
              t)))

      )


(test clover.tests.unify.nested-arity-mismatch
      ;; 不具合1: アリティ不一致が単一化検査をすり抜ける（健全性の問題）。
      ;;
      ;; src/unify.lisp の %%collect-disagreement-set の (fterm fterm) メソッドで、
      ;; ローカル変数 length2 が (length args2) ではなく (length args1) で束縛されて
      ;; いるため、length1 = length2 が常に真となり、引数個数（アリティ）の不一致
      ;; 検査が機能しない。
      ;;
      ;; 低レベルテスト: %%collect-disagreement-set を直接呼ぶ。
      ;; f/1 と f/2 はアリティが異なるため、本来 unmatching-fterm-error が送出される
      ;; べき。現状はバグにより検査をすり抜け、mapcan が短い引数列 (x) で打ち切られて
      ;; (x->A のみ収集し B を捨てて) エラーにならない。よってこのアサーションは FAIL。
      (signals unmatching-fterm-error
        (clover.unify::%%collect-disagreement-set
          (fterm 'f (list (vterm 'x)))
          (fterm 'f (list (constant 'A) (constant 'B)))))

      ;; 高レベルテスト: 公開 API find-most-general-unifier-set 経由。
      ;; トップレベル g/2 同士はアリティが一致するため上位の検査を通過するが、
      ;; 内側の f/1 と f/2 はアリティが異なる。本来 ununifiable-error になるべき
      ;; だが、現状は誤って単一化に成功する。よってこのアサーションは FAIL。
      (signals ununifiable-error
        (find-most-general-unifier-set
          (fterm 'g (list (fterm 'f (list (vterm 'x))) (constant 'C)))
          (fterm 'g (list (fterm 'f (list (constant 'A) (constant 'B))) (constant 'C)))))

      ;; 対照群: 内側アリティが一致し、本当に単一化可能なケースは従来どおり成功する。
      ;; 修正後も、上の2つの signals とこの is が同時に通る形になるべき。
      (is
        (let ((us
                (find-most-general-unifier-set
                  (fterm 'g (list (fterm 'f (list (vterm 'x))) (constant 'C)))
                  (fterm 'g (list (fterm 'f (list (constant 'A))) (constant 'C))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (constant 'A))))))))


(test clover.tests.unify.find-most-general-unifier-set.test1

      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (literal nil 'P (list (vterm 'x) (vterm 'x)))
                  (literal t   'P (list (vterm 'y) (fterm 'f (list (vterm 'w))))))))

          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'f (list (vterm 'w))))
                    (unifier (vterm 'y) (fterm 'f (list (vterm 'w)))))))))
      
      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (literal nil 'P (list (vterm 'x) (vterm 'x) (vterm 'y)))
                  (literal t   'P (list (vterm 'w) (vterm 'v) (fterm 'f (list (vterm 'v))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'v))
                    (unifier (vterm 'y) (fterm 'f (list (vterm 'v))))
                    (unifier (vterm 'w) (vterm 'v)))))))
      
      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (literal nil 'P (list (vterm 'x) (vterm 'x)))
                  (literal t   'P (list (vterm 'w) (vterm 'v))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (vterm 'v))
                    (unifier (vterm 'w) (vterm 'v)))))))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'Q (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-error (e) t)))

      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal t 'P (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-error (e) t)))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'P (list (vterm 'x) (fterm 'f (list (vterm 'x)))))
              (literal t   'P (list (vterm 'w) (vterm 'w))))
          (ununifiable-error (e) t)))
      
      (is 
        (handler-case 
            (find-most-general-unifier-set 
              (literal nil 'P (list (vterm 'x) (vterm 'x)))
              (literal t   'P (list (fterm 'f (list (vterm 'w))) 
                                    (fterm 'g (list (vterm 'v))))))
          (ununifiable-error (e) t)))

      )


(test clover.tests.unify.subsumption-clause-p.test1
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x)))))
              (clause (list (literal nil 'P 
                                     (list (fterm 'f (list (vterm 'y)))))
                            (literal t 'Q
                                     (list (vterm 'y) (vterm 'z)))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x)))))
              (clause (list (literal nil 'P 
                                     (list (fterm 'g (list (vterm 'y)))))
                            (literal nil 'Q
                                     (list (vterm 'y)))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))))
              (clause (list (literal nil 'P 
                                     (list (constant 'A ) (constant 'B )))
                            (literal nil 'Q
                                     (list (constant 'A ) (constant 'B )))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))))
              (clause (list (literal nil 'P 
                                     (list (constant 'A ) 
                                           (fterm 'f (list (vterm 'z)))))))))
      (is (subsumption-clause-p
              (clause (list (literal nil 'P 
                                     (list (vterm 'x) (vterm 'y)))
                            (literal nil 'Q
                                     (list (vterm 'y) (vterm 'x)))))

              (clause (list (literal nil 'P 
                                     (list (constant 'B ) 
                                           (fterm 'f (list (vterm 'z)
                                                           (fterm 'g (list (vterm 'w)))))))
                            (literal nil 'Q
                                     (list (fterm 'f (list (vterm 'z)
                                                           (fterm 'g (list (vterm 'w)))))
                                           (constant 'B )))
                            (literal t 'R 
                                     (list (constant 'A )
                                           (fterm 'f (list (vterm 'z))))))))))

(test clover.tests.unify.subsumption-clause-p.test2
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (vterm 'x)))
                               (literal nil 'Q
                                        (list (vterm 'x) (vterm 'y)))))
                 (clause (list (literal nil 'P
                                        (list (constant 'A )))
                               (literal nil 'Q
                                        (list (vterm 'w) (constant 'B )))
                               (literal nil 'R
                                        (list (vterm 'w) (vterm 'z))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (vterm 'x) (vterm 'y)))))
                 (clause (list (literal t 'P
                                        (list (constant 'A ) (constant 'B )))
                               (literal nil 'Q
                                        (list (constant 'A ) (constant 'B ))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P
                                        (list (constant 'A )))))
                 (clause (list (literal nil 'P
                                        (list (vterm 'x)))
                               (literal t 'Q
                                        (list (vterm 'x))))))))
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P nil)))
                 (clause (list (literal t 'P nil))))))

      (is (not (subsumption-clause-p
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'Q (list (vterm 'x) (vterm 'z)))))
              (clause (list (literal nil 'P (list (constant 'A )))
                            (literal nil 'Q (list (vterm 'w) (vterm 'u)))
                            (literal nil 'R (list (vterm 'w))))))))
      )

(test clover.tests.unify.subsumption-clause-p.test3
      (is (subsumption-clause-p
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
              (clause (list (literal nil 'P (list (constant 'A )))
                            (literal nil 'Q (list (vterm 'w) (vterm 'u)))
                            (literal nil 'R (list (vterm 'w)))))))
      )


(test clover.tests.unify.alphabet=.test1
      (is (alphabet=
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (vterm 'z)))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (vterm 'v)))))))))
      (is (alphabet=
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (constant 'A )))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (constant 'A )))))))))
      (is (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v))))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'g (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (constant'A )))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet= 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'Q (list (vterm 'v)))
                          (literal nil 'P (list (vterm 'w) (vterm 'u))))))))
)


(test clover.tests.unify.alphabet=.test2
      (is (not (alphabet= 
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
                                                     (vterm 'w))))))))
      )


(test clover.tests.unify.alphabet=.test3
      (let* ((clause1
              (clause
                (list 
                  (literal nil 'P (list (vterm 'x)
                                        (vterm 'y)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'x))))))))
            (clause2
              (clause
                (list 
                  (literal t 'R (list (vterm 'z))))))
            (clause3
              (clause
                (list 
                  (literal nil 'P (list (vterm 'w)
                                        (vterm 'u)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'w))))))))
            (clause4
              (clause
                (list 
                  (literal t 'R (list (vterm 'v))))))
            (clause-set1
              (clause-set
                (list clause1 clause2)))
            (clause-set2
              (clause-set 
                (list clause4 clause3))))
        (is (alphabet= clause-set2 clause-set1))))

(test clover.tests.unify.alphabet=.test4
      (let* ((clause1
              (clause
                (list 
                  (literal nil 'P (list (vterm 'x)
                                        (vterm 'y)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'x))))))))
            (clause2
              (clause
                (list 
                  (literal t 'R (list (vterm 'z))))))
            (clause3
              (clause
                (list 
                  (literal nil 'P (list (vterm 'w)
                                        (vterm 'u)))
                  (literal nil 'Q (list (fterm 'F (list (vterm 'u))))))))
            (clause4
              (clause
                (list 
                  (literal t 'R (list (vterm 'v))))))
            (clause-set1
              (clause-set
                (list clause1 clause2)))
            (clause-set2
              (clause-set 
                (list clause4 clause3))))
        (is (not (alphabet= clause-set2 clause-set1)))))

(test clover.tests.unify.alphabet=.test5
      (is (not (alphabet= 
                 (fterm 'f (list (vterm 'x) (vterm 'y)))
                 (fterm 'f (list (vterm 'w) (vterm 'w))))))
      (is (alphabet= 
            (fterm 'f (list (vterm 'x) (vterm 'y)))
            (fterm 'f (list (vterm 'u) (vterm 'v)))))
      (is (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'w)))))))
      (is (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y) (fterm 'h (list (vterm 'z) (fterm 'i (list (vterm 'p)))))))))
            (fterm 'f (list (vterm 'u) (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'w) (fterm 'i (list (vterm 'k)))))))))))
      (is (not (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y) (fterm 'h (list (vterm 'z) (fterm 'i (list (vterm 'p)))))))))
            (fterm 'f (list (vterm 'u) (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'w) (vterm 'u))))))))))
      (is (not (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (vterm 'u))))))
      (is (not (alphabet= 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'z))))))))
      (is (not (alphabet= 
            (fterm 'f (list (fterm 'g (list (vterm 'y))) (vterm 'x) ))
            (fterm 'f (list (fterm 'g (list (vterm 'z))) (vterm 'z) )))))
      (is (not (alphabet= 
            (fterm 'f (list (fterm 'g (list (vterm 'y))) (vterm 'x) ))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'z))) )))))
      (is (alphabet= (constant 'A) (constant 'A)))
      (is (alphabet= (vterm 'x) (vterm 'y)))

      (is (not (alphabet= (constant 'B) (constant 'A))))
      (is (alphabet= (vterm 'x) (vterm 'x)))

      (is (not (alphabet= (constant 'B) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet= (vterm 'x) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet= (vterm 'y) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet= (constant 'B) (vterm 'b))))
      (is (alphabet= (constant 'B) (constant 'B)))
      ;; (fterm 'B nil) は現在 constant に正規化されるため、これは実質
      ;; constant 同士の比較になる（ゼロ引数 fterm はもう生成されない）。
      (is (typep (fterm 'B nil) 'constant))
      (is (alphabet= (constant 'B) (fterm 'B nil))))

(test clover.tests.unify.alphabet=.test6
      (let ((criteria1
              (rewrite-rule
                (fterm 'f (list (vterm 'x) (vterm 'y)))
                (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
        (is 
          (alphabet= criteria1
                     (rewrite-rule
                       (fterm 'f (list (vterm 'u) (vterm 'v)))
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'z)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'w) (vterm 'w)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'y)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (alphabet=
            (rewrite-rule
              (fterm 'f (list (vterm 'x)))
              (vterm 'x))
            (rewrite-rule
              (fterm 'f (list (vterm 'z)))
              (vterm 'z))))
        (is 
          (not (alphabet=
            (rewrite-rule
              (fterm 'f (list (vterm 'x)))
              (vterm 'x))
            (rewrite-rule
              (fterm 'f (list (vterm 'y)))
              (vterm 'z)))))
        (is 
          (alphabet=
            (rewrite-rule
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (rewrite-rule
              (vterm 'z)
              (fterm 'f (list (vterm 'z))))))
        (is 
          (not (alphabet=
            (rewrite-rule
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (rewrite-rule
              (vterm 'z)
              (fterm 'f (list (vterm 'w)))))))
        (is 
          (alphabet=
            (rewrite-rule
              (vterm 'x)
              (vterm 'x))
            (rewrite-rule
              (vterm 'z)
              (vterm 'z))))
        (is 
          (alphabet=
            (rewrite-rule
              (vterm 'y)
              (vterm 'z))
            (rewrite-rule
              (vterm 'w)
              (vterm 't))))
        (is 
          (not (alphabet=
            (rewrite-rule
              (vterm 'x)
              (vterm 'x))
            (rewrite-rule
              (vterm 'y)
              (vterm 'z)))))))

(test clover.tests.unify.alphabet=.test7
      (let ((criteria1
              (equation nil
                (fterm 'f (list (vterm 'x) (vterm 'y)))
                (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
        (is 
          (alphabet= criteria1
                     (equation nil
                       (fterm 'f (list (vterm 'u) (vterm 'v)))
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v))))))))
        (is 
          (alphabet= criteria1
                     (equation nil
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v)))))
                       (fterm 'f (list (vterm 'u) (vterm 'v))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'z)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'w) (vterm 'w)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'y)))))))))
        (is 
          (not 
            (alphabet= criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not (alphabet=
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation t
              (vterm 'z)
              (fterm 'f (list (vterm 'z)))))))
        (is 
          (alphabet=
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation nil
              (vterm 'z)
              (fterm 'f (list (vterm 'z))))))
        (is 
          (not (alphabet=
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation nil
              (vterm 'z)
              (fterm 'f (list (vterm 'w)))))))
        (is 
          (alphabet=
            (equation nil
              (vterm 'x)
              (vterm 'x))
            (equation nil
              (vterm 'z)
              (vterm 'z))))
        (is 
          (alphabet=
            (equation nil
              (vterm 'y)
              (vterm 'z))
            (equation nil
              (vterm 'w)
              (vterm 't))))))

(test clover.tests.unify.alphabet=.test8
      (is 
          (not (alphabet=
            (equation nil
              (fterm 'plus (list (vterm 'x) (vterm 'y)))
              (fterm 'plus (list (fterm 'plus (list (vterm 'x) 
                                                    (constant 'ZERO)))
                                 (vterm 'y))))
            (equation nil
              (fterm 'plus (list (fterm 'plus (list (fterm 'inv (list (vterm 'u)))
                                                    (constant 'ZERO)))
                                 (vterm 'u)))
              (constant 'ZERO)
              ))))
      )

(test clover.tests.unify.alphabet=.test9
      (is 
          (not (alphabet=
            (equation nil
              (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
              (fterm 'plus (list (fterm 'inv (list (fterm 'inv (list (vterm 'x)))))
                                 (constant 'ZERO))))
            (equation nil
              (fterm 'plus (list (constant 'ZERO) (vterm 'u)))
              (vterm 'u)
              )))))


(test clover.tests.unify.alphabet=.reflexivity
      ;; あるべき仕様: alphabet= は「変数名の付け替えを除いた同値」を表す同値関係で
      ;; あり、任意の規則/等式 r に対して反射律 (alphabet= r r) = 真 が成り立つべき。
      ;;
      ;; 事実: %alphabet=-for-rule-or-eq の vterm->fterm 分岐は、dst 同士の最汎
      ;; 単一化子に (src1 -> src2) が含まれるかを member で検査する。src の変数が
      ;; dst に出現しない場合、その単一化子集合に src 変数が現れず member 検査が
      ;; 失敗するため、自己比較でも NIL を返す。よって下の「違反」群は現実装で
      ;; FAIL する（反射律が破れている）。

      ;; --- 反射律が成立する形状（対照群・現実装でも真）---
      (let ((r-ff   (rewrite-rule (fterm 'f (list (vterm 'x) (vterm 'y)))
                                  (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y)))))))
            (r-fv   (rewrite-rule (fterm 'f (list (vterm 'x))) (vterm 'x)))
            (r-vv   (rewrite-rule (vterm 'x) (vterm 'y)))
            (r-cf   (rewrite-rule (constant 'A) (fterm 'f (list (vterm 'x)))))
            (e-ff   (equation nil (fterm 'f (list (vterm 'x) (vterm 'y)))
                                  (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y)))))))
            (e-comm (equation nil (fterm 'plus (list (vterm 'x) (vterm 'y)))
                                  (fterm 'plus (list (vterm 'y) (vterm 'x))))))
        (is (alphabet= r-ff   r-ff))
        (is (alphabet= r-fv   r-fv))
        (is (alphabet= r-vv   r-vv))
        (is (alphabet= r-cf   r-cf))
        (is (alphabet= e-ff   e-ff))
        (is (alphabet= e-comm e-comm)))

      ;; --- 反射律が破れている形状（変数 <-> その変数を含まない項）---
      ;; いずれも自分自身との比較。本来 真 であるべきだが現実装は NIL を返し FAIL する。
      (let ((r-vf-nofree (rewrite-rule (vterm 'x) (fterm 'f (list (vterm 'y)))))  ; x -> f(y)
            (r-vc        (rewrite-rule (vterm 'x) (constant 'A)))                 ; x -> A
            (e-vc        (equation nil (vterm 'x) (constant 'A)))                 ; x = A
            (e-vf-nofree (equation nil (vterm 'x) (fterm 'f (list (vterm 'y)))))) ; x = f(y)
        (is (alphabet= r-vf-nofree r-vf-nofree))
        (is (alphabet= r-vc        r-vc))
        (is (alphabet= e-vc        e-vc))
        (is (alphabet= e-vf-nofree e-vf-nofree))))


(test clover.tests.unify.alphabet=.constant-dst
      ;; %alphabet=-for-rule-or-eq の「定数を含む規則/等式」のあるべき挙動。
      ;;
      ;; 事実: constant は types.lisp で (:include fterm)。%alphabet=-for-rule-or-eq
      ;; の typecase は (fterm ...) が (constant ...) より先にあるため、定数値は
      ;; (fterm ...) 節に吸い込まれ (constant ...) 節へは到達しない。その結果、
      ;; src=変数・dst=定数の規則/等式は vterm->fterm 経路を通り、mgu(定数,定数) が
      ;; 空集合になるため member 検査が空集合に対して失敗し、本来 T であるべき
      ;; ところ NIL を返す（＝下の [A][B] は現実装では FAIL する）。
      ;;
      ;; あるべき仕様（推測・確度高、要設計確認）: 定数は変数を含まないため
      ;; src<->dst の変数対応制約が無く、各辺が個別に alphabet= であれば規則/等式
      ;; としてもアルファ同値（T）とすべき。

      ;; --- 現実装では FAIL する（本来 T であるべき）---
      ;; [A] x -> A と z -> A は変数 x/z を付け替えれば一致する同一規則。
      (is (alphabet=
            (rewrite-rule (vterm 'x) (constant 'A))
            (rewrite-rule (vterm 'z) (constant 'A))))
      ;; [B] 等式版 x = A と z = A も同様に同値であるべき。
      (is (alphabet=
            (equation nil (vterm 'x) (constant 'A))
            (equation nil (vterm 'z) (constant 'A))))

      ;; --- 対照群（現実装でも T。修正後も維持されるべき）---
      ;; [C] fterm-src + 定数-dst。
      (is (alphabet=
            (rewrite-rule (fterm 'f (list (vterm 'x))) (constant 'A))
            (rewrite-rule (fterm 'f (list (vterm 'z))) (constant 'A))))
      ;; [E] 定数-src + fterm-dst。
      (is (alphabet=
            (rewrite-rule (constant 'A) (fterm 'f (list (vterm 'x))))
            (rewrite-rule (constant 'A) (fterm 'f (list (vterm 'y))))))

      ;; --- 過剰許容の防止（修正後も NIL を維持すべき負のケース）---
      ;; 定数が異なれば不一致。
      (is (not (alphabet=
                 (rewrite-rule (vterm 'x) (constant 'A))
                 (rewrite-rule (vterm 'z) (constant 'B)))))
      (is (not (alphabet=
                 (equation nil (vterm 'x) (constant 'A))
                 (equation nil (vterm 'z) (constant 'B)))))
      ;; src の変数構造が異なれば、dst が定数でも src を無視して T にしてはいけない。
      (is (not (alphabet=
                 (rewrite-rule (fterm 'f (list (vterm 'x) (vterm 'x))) (constant 'A))
                 (rewrite-rule (fterm 'f (list (vterm 'u) (vterm 'v))) (constant 'A))))))


(test clover.tests.unify.find-most-general-unifier-set.test2

      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
                  (fterm 'plus (list (vterm 'u) (fterm 'inv (list (vterm 'u))))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'inv (list (constant 'ZERO))))
                    (unifier (vterm 'u) (constant 'ZERO)))))))
      (is 
        (let ((us 
                (find-most-general-unifier-set 
                  (fterm 'plus (list (vterm 'u) (fterm 'inv (list (vterm 'u)))))
                  (fterm 'plus (list (constant 'ZERO) (vterm 'x))))))
          (unifier-set=
            us
            (unifier-set
              (list (unifier (vterm 'x) (fterm 'inv (list (constant 'ZERO))))
                    (unifier (vterm 'u) (constant 'ZERO)))))))
      )

