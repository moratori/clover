(defpackage clover.tests.unify
  (:use :cl
        :clover.conditions
        :clover.unify
        :clover.types
        :clover.logical-predicates
        :1am)
  (:import-from :clover.tests.util
                :skip-test
                )
  (:import-from :clover.equality
                :term=
                :unifier-set=
                :clause=))
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


(test clover.tests.unify.subsumption-clause-p.reflexivity
      ;; 横展開調査で判明した不具合: subsumption-clause-p が反射律を満たさない。
      ;; 任意の節は恒等代入で自分自身を包摂するはずだが、{P(x),P(A)} 型
      ;; （変数のみのリテラル＋別リテラル）は自分自身を包摂しない。
      ;; 原因: %%collect-disagreement-set((vterm)(vterm)) が同一変数でも恒等
      ;; 単一化子 x:=x を生成し、貪欲マッチで P(A) も先頭 P(x) に当たって x:=A を
      ;; 要求 → consistent-unifier-set-p が x:=x と x:=A の衝突で偽 → NIL。
      ;; → 下記は現実装では FAIL する。
      (let ((c (clause (list (literal nil 'P (list (vterm 'x)))
                             (literal nil 'P (list (constant 'A)))))))
        (is (subsumption-clause-p c c)))
      ;; 対照: 変数のみの節は反射律 OK（現状でも T）
      (let ((c (clause (list (literal nil 'P (list (vterm 'x)))
                             (literal nil 'P (list (vterm 'y)))))))
        (is (subsumption-clause-p c c))))

(test clover.tests.unify.subsumption-clause-p.sign-sensitivity
      ;; 横展開調査で判明した不具合: subsumption-clause-p がリテラルの符号(negation)を
      ;; 無視して貪欲マッチするため偽陰性になる。
      ;; {P(x)} は P(x)->P(B) で {¬P(A), P(B)} を包摂する（本来 T）が、符号を無視して
      ;; 先頭の ¬P(A) に当たり x:=A に確定 → 後段の符号厳密な clause-subset が満たせず
      ;; NIL を返す。→ 下記は現実装では FAIL する。
      (is (subsumption-clause-p
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal t 'P (list (constant 'A)))
                          (literal nil 'P (list (constant 'B)))))))
      ;; 対照: 同じ意味で並び順を入れ替える（P(B) を先頭に）と現状でも T。
      ;; ＝結果が節の並び順に依存していることの実証。
      (is (subsumption-clause-p
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'B)))
                          (literal t 'P (list (constant 'A))))))))

(test clover.tests.unify.subsumption-clause-p.greedy-incompleteness
      ;; 横展開調査で判明した不具合: subsumption-clause-p の内側ループが
      ;; 「先頭に出会った単一化可能な lit2 へ確定」する貪欲マッチで、選択に
      ;; バックトラックが無いため、包摂判定として不完全（偽陰性を返し得る）。
      ;;
      ;;   C = {P(x), Q(x)}        （x を共有）
      ;;   D = {P(A), P(B), Q(B)}
      ;;
      ;; 真の包摂: θ = {x:=B} で Cθ = {P(B), Q(B)} ⊆ D。よって C は D を包摂する（本来 T）。
      ;; 貪欲マッチの挙動: P(x) を D 先頭の P(A) に当てて x:=A を確定 →
      ;;   Q(x) は Q(B) で x:=B を要求 → consistent-unifier-set-p が矛盾で偽 → NIL。
      ;;   正解は P(x) を 2 番目の P(B) に当てることだが、先頭確定のため到達できない。
      ;; → 下記は（恒等単一化子・符号の修正後も）依然 FAIL する。
      (is (subsumption-clause-p
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'A)))
                          (literal nil 'P (list (constant 'B)))
                          (literal nil 'Q (list (constant 'B)))))))
      ;; 対照: D を並べ替えて P(B) を先頭にすると、貪欲マッチでも P(x)->P(B) を
      ;; 先に確定でき、現状でも T になる。＝結果が D の並び順に依存することの実証。
      (is (subsumption-clause-p
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'B)))
                          (literal nil 'P (list (constant 'A)))
                          (literal nil 'Q (list (constant 'B))))))))

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


(test clover.tests.unify.alphabet-equivalent-p.test1
      (is (alphabet-equivalent-p
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (vterm 'z)))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (vterm 'v)))))))))
      (is (alphabet-equivalent-p
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))
                                                (vterm 'y)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'y)))
                                              (fterm 'g (list (constant 'A )))))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'w)))
                                                (vterm 'u)))
                          (literal t 'Q (list (fterm 'g (list (vterm 'u)))
                                              (fterm 'g (list (constant 'A )))))))))
      (is (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u)))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'v))))))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'g (list (vterm 'v)))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x)))))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (fterm 'f (list (constant'A )))))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (vterm 'v)))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'P (list (constant 'A )))
                          (literal nil 'Q (list (vterm 'w) (vterm 'u))))))))
      (is (not (alphabet-equivalent-p 
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y) (vterm 'z)))))
            (clause (list (literal nil 'Q (list (vterm 'v)))
                          (literal nil 'P (list (vterm 'w) (vterm 'u))))))))
)


(test clover.tests.unify.alphabet-equivalent-p.test2
      (is (not (alphabet-equivalent-p 
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


(test clover.tests.unify.alphabet-equivalent-p.test3
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
        (is (alphabet-equivalent-p clause-set2 clause-set1))))

(test clover.tests.unify.alphabet-equivalent-p.test4
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
        (is (not (alphabet-equivalent-p clause-set2 clause-set1)))))

(test clover.tests.unify.alphabet-equivalent-p.test5
      (is (not (alphabet-equivalent-p 
                 (fterm 'f (list (vterm 'x) (vterm 'y)))
                 (fterm 'f (list (vterm 'w) (vterm 'w))))))
      (is (alphabet-equivalent-p 
            (fterm 'f (list (vterm 'x) (vterm 'y)))
            (fterm 'f (list (vterm 'u) (vterm 'v)))))
      (is (alphabet-equivalent-p 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'w)))))))
      (is (alphabet-equivalent-p 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y) (fterm 'h (list (vterm 'z) (fterm 'i (list (vterm 'p)))))))))
            (fterm 'f (list (vterm 'u) (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'w) (fterm 'i (list (vterm 'k)))))))))))
      (is (not (alphabet-equivalent-p 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y) (fterm 'h (list (vterm 'z) (fterm 'i (list (vterm 'p)))))))))
            (fterm 'f (list (vterm 'u) (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'w) (vterm 'u))))))))))
      (is (not (alphabet-equivalent-p 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (vterm 'u))))))
      (is (not (alphabet-equivalent-p 
            (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'y)))))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'z))))))))
      (is (not (alphabet-equivalent-p 
            (fterm 'f (list (fterm 'g (list (vterm 'y))) (vterm 'x) ))
            (fterm 'f (list (fterm 'g (list (vterm 'z))) (vterm 'z) )))))
      (is (not (alphabet-equivalent-p 
            (fterm 'f (list (fterm 'g (list (vterm 'y))) (vterm 'x) ))
            (fterm 'f (list (vterm 'z) (fterm 'g (list (vterm 'z))) )))))
      (is (alphabet-equivalent-p (constant 'A) (constant 'A)))
      (is (alphabet-equivalent-p (vterm 'x) (vterm 'y)))

      (is (not (alphabet-equivalent-p (constant 'B) (constant 'A))))
      (is (alphabet-equivalent-p (vterm 'x) (vterm 'x)))

      (is (not (alphabet-equivalent-p (constant 'B) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet-equivalent-p (vterm 'x) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet-equivalent-p (vterm 'y) (fterm 'f (list (vterm 'x))))))
      (is (not (alphabet-equivalent-p (constant 'B) (vterm 'b))))
      (is (alphabet-equivalent-p (constant 'B) (constant 'B)))
      ;; (fterm 'B nil) は現在 constant に正規化されるため、これは実質
      ;; constant 同士の比較になる（ゼロ引数 fterm はもう生成されない）。
      (is (typep (fterm 'B nil) 'constant))
      (is (alphabet-equivalent-p (constant 'B) (fterm 'B nil))))

(test clover.tests.unify.alphabet-equivalent-p.test6
      (let ((criteria1
              (rewrite-rule
                (fterm 'f (list (vterm 'x) (vterm 'y)))
                (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
        (is 
          (alphabet-equivalent-p criteria1
                     (rewrite-rule
                       (fterm 'f (list (vterm 'u) (vterm 'v)))
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'z)))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'w) (vterm 'w)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'y)))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (rewrite-rule
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (alphabet-equivalent-p
            (rewrite-rule
              (fterm 'f (list (vterm 'x)))
              (vterm 'x))
            (rewrite-rule
              (fterm 'f (list (vterm 'z)))
              (vterm 'z))))
        (is 
          (not (alphabet-equivalent-p
            (rewrite-rule
              (fterm 'f (list (vterm 'x)))
              (vterm 'x))
            (rewrite-rule
              (fterm 'f (list (vterm 'y)))
              (vterm 'z)))))
        (is 
          (alphabet-equivalent-p
            (rewrite-rule
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (rewrite-rule
              (vterm 'z)
              (fterm 'f (list (vterm 'z))))))
        (is 
          (not (alphabet-equivalent-p
            (rewrite-rule
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (rewrite-rule
              (vterm 'z)
              (fterm 'f (list (vterm 'w)))))))
        (is 
          (alphabet-equivalent-p
            (rewrite-rule
              (vterm 'x)
              (vterm 'x))
            (rewrite-rule
              (vterm 'z)
              (vterm 'z))))
        (is 
          (alphabet-equivalent-p
            (rewrite-rule
              (vterm 'y)
              (vterm 'z))
            (rewrite-rule
              (vterm 'w)
              (vterm 't))))
        (is 
          (not (alphabet-equivalent-p
            (rewrite-rule
              (vterm 'x)
              (vterm 'x))
            (rewrite-rule
              (vterm 'y)
              (vterm 'z)))))))

(test clover.tests.unify.alphabet-equivalent-p.test7
      (let ((criteria1
              (equation nil
                (fterm 'f (list (vterm 'x) (vterm 'y)))
                (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'y))))))))
        (is 
          (alphabet-equivalent-p criteria1
                     (equation nil
                       (fterm 'f (list (vterm 'u) (vterm 'v)))
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v))))))))
        (is 
          (alphabet-equivalent-p criteria1
                     (equation nil
                       (fterm 'g (list (vterm 'u) (fterm 'h (list (vterm 'v)))))
                       (fterm 'f (list (vterm 'u) (vterm 'v))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'z)))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'w) (vterm 'w)))
                         (fterm 'g (list (vterm 'w) (fterm 'h (list (vterm 'y)))))))))
        (is 
          (not 
            (alphabet-equivalent-p criteria1
                       (equation nil
                         (fterm 'f (list (vterm 'u) (vterm 'v)))
                         (fterm 'g (list (vterm 'v) (fterm 'h (list (vterm 'u)))))))))
        (is 
          (not (alphabet-equivalent-p
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation t
              (vterm 'z)
              (fterm 'f (list (vterm 'z)))))))
        (is 
          (alphabet-equivalent-p
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation nil
              (vterm 'z)
              (fterm 'f (list (vterm 'z))))))
        (is 
          (not (alphabet-equivalent-p
            (equation nil
              (vterm 'x)
              (fterm 'f (list (vterm 'x)))
              )
            (equation nil
              (vterm 'z)
              (fterm 'f (list (vterm 'w)))))))
        (is 
          (alphabet-equivalent-p
            (equation nil
              (vterm 'x)
              (vterm 'x))
            (equation nil
              (vterm 'z)
              (vterm 'z))))
        (is 
          (alphabet-equivalent-p
            (equation nil
              (vterm 'y)
              (vterm 'z))
            (equation nil
              (vterm 'w)
              (vterm 't))))))

(test clover.tests.unify.alphabet-equivalent-p.test8
      (is 
          (not (alphabet-equivalent-p
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

(test clover.tests.unify.alphabet-equivalent-p.test9
      (is 
          (not (alphabet-equivalent-p
            (equation nil
              (fterm 'plus (list (constant 'ZERO) (vterm 'x)))
              (fterm 'plus (list (fterm 'inv (list (fterm 'inv (list (vterm 'x)))))
                                 (constant 'ZERO))))
            (equation nil
              (fterm 'plus (list (constant 'ZERO) (vterm 'u)))
              (vterm 'u)
              )))))


(test clover.tests.unify.alphabet-equivalent-p.reflexivity
      ;; あるべき仕様: alphabet-equivalent-p は「変数名の付け替えを除いた同値」を表す同値関係で
      ;; あり、任意の規則/等式 r に対して反射律 (alphabet-equivalent-p r r) = 真 が成り立つべき。
      ;;
      ;; 事実: %alphabet-equivalent-p-for-rule-or-eq の vterm->fterm 分岐は、dst 同士の最汎
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
        (is (alphabet-equivalent-p r-ff   r-ff))
        (is (alphabet-equivalent-p r-fv   r-fv))
        (is (alphabet-equivalent-p r-vv   r-vv))
        (is (alphabet-equivalent-p r-cf   r-cf))
        (is (alphabet-equivalent-p e-ff   e-ff))
        (is (alphabet-equivalent-p e-comm e-comm)))

      ;; --- 反射律が破れている形状（変数 <-> その変数を含まない項）---
      ;; いずれも自分自身との比較。本来 真 であるべきだが現実装は NIL を返し FAIL する。
      (let ((r-vf-nofree (rewrite-rule (vterm 'x) (fterm 'f (list (vterm 'y)))))  ; x -> f(y)
            (r-vc        (rewrite-rule (vterm 'x) (constant 'A)))                 ; x -> A
            (e-vc        (equation nil (vterm 'x) (constant 'A)))                 ; x = A
            (e-vf-nofree (equation nil (vterm 'x) (fterm 'f (list (vterm 'y)))))) ; x = f(y)
        (is (alphabet-equivalent-p r-vf-nofree r-vf-nofree))
        (is (alphabet-equivalent-p r-vc        r-vc))
        (is (alphabet-equivalent-p e-vc        e-vc))
        (is (alphabet-equivalent-p e-vf-nofree e-vf-nofree))))


(test clover.tests.unify.alphabet-equivalent-p.constant-dst
      ;; %alphabet-equivalent-p-for-rule-or-eq の「定数を含む規則/等式」のあるべき挙動。
      ;;
      ;; 事実: constant は types.lisp で (:include fterm)。%alphabet-equivalent-p-for-rule-or-eq
      ;; の typecase は (fterm ...) が (constant ...) より先にあるため、定数値は
      ;; (fterm ...) 節に吸い込まれ (constant ...) 節へは到達しない。その結果、
      ;; src=変数・dst=定数の規則/等式は vterm->fterm 経路を通り、mgu(定数,定数) が
      ;; 空集合になるため member 検査が空集合に対して失敗し、本来 T であるべき
      ;; ところ NIL を返す（＝下の [A][B] は現実装では FAIL する）。
      ;;
      ;; あるべき仕様（推測・確度高、要設計確認）: 定数は変数を含まないため
      ;; src<->dst の変数対応制約が無く、各辺が個別に alphabet-equivalent-p であれば規則/等式
      ;; としてもアルファ同値（T）とすべき。

      ;; --- 現実装では FAIL する（本来 T であるべき）---
      ;; [A] x -> A と z -> A は変数 x/z を付け替えれば一致する同一規則。
      (is (alphabet-equivalent-p
            (rewrite-rule (vterm 'x) (constant 'A))
            (rewrite-rule (vterm 'z) (constant 'A))))
      ;; [B] 等式版 x = A と z = A も同様に同値であるべき。
      (is (alphabet-equivalent-p
            (equation nil (vterm 'x) (constant 'A))
            (equation nil (vterm 'z) (constant 'A))))

      ;; --- 対照群（現実装でも T。修正後も維持されるべき）---
      ;; [C] fterm-src + 定数-dst。
      (is (alphabet-equivalent-p
            (rewrite-rule (fterm 'f (list (vterm 'x))) (constant 'A))
            (rewrite-rule (fterm 'f (list (vterm 'z))) (constant 'A))))
      ;; [E] 定数-src + fterm-dst。
      (is (alphabet-equivalent-p
            (rewrite-rule (constant 'A) (fterm 'f (list (vterm 'x))))
            (rewrite-rule (constant 'A) (fterm 'f (list (vterm 'y))))))

      ;; --- 過剰許容の防止（修正後も NIL を維持すべき負のケース）---
      ;; 定数が異なれば不一致。
      (is (not (alphabet-equivalent-p
                 (rewrite-rule (vterm 'x) (constant 'A))
                 (rewrite-rule (vterm 'z) (constant 'B)))))
      (is (not (alphabet-equivalent-p
                 (equation nil (vterm 'x) (constant 'A))
                 (equation nil (vterm 'z) (constant 'B)))))
      ;; src の変数構造が異なれば、dst が定数でも src を無視して T にしてはいけない。
      (is (not (alphabet-equivalent-p
                 (rewrite-rule (fterm 'f (list (vterm 'x) (vterm 'x))) (constant 'A))
                 (rewrite-rule (fterm 'f (list (vterm 'u) (vterm 'v))) (constant 'A))))))


(test clover.tests.unify.alphabet-equivalent-p.rule-vterm-symmetry
      ;; vterm->vterm 規則の「同変数 vs 別変数」は、alphabet-equivalent-p が対称な同値関係である以上、
      ;; どちらを第1引数に置いても結果は同じ（NIL）であるべき。
      ;;
      ;; 事実: 既存 test6(633-640行) は (同変数, 別変数) の向きだけを検査しており、
      ;; 逆向き (別変数, 同変数) が未カバー。%alphabet-equivalent-p-for-rule-or-eq の vterm->vterm 分岐は
      ;;   (if (term= rule1-src rule1-dst) (term= rule2-src rule2-dst) t)
      ;; と rule1 が別変数のとき rule2 を見ずに t を返すため、向きに依存して偽陽性になる。

      ;; --- 対称性を保証する両向き（本来どちらも NIL）---
      ;; [A] 別変数 x->y vs 同変数 z->z : 現実装は T を返し FAIL する。
      (is (not (alphabet-equivalent-p (rewrite-rule (vterm 'x) (vterm 'y))
                          (rewrite-rule (vterm 'z) (vterm 'z)))))
      ;; [B] 逆向き 同変数 z->z vs 別変数 x->y : こちらは現実装でも NIL（対称性の対照）。
      (is (not (alphabet-equivalent-p (rewrite-rule (vterm 'z) (vterm 'z))
                          (rewrite-rule (vterm 'x) (vterm 'y)))))

      ;; --- 正例（両向きとも T。修正後も維持されるべき）---
      ;; 別変数同士・同変数同士はアルファ同値。
      (is (alphabet-equivalent-p (rewrite-rule (vterm 'x) (vterm 'y))
                     (rewrite-rule (vterm 'z) (vterm 'w))))
      (is (alphabet-equivalent-p (rewrite-rule (vterm 'x) (vterm 'x))
                     (rewrite-rule (vterm 'z) (vterm 'z)))))


(test clover.tests.unify.alphabet-equivalent-p.rule-free-variable
      ;; src 変数が dst に出現しない形（自由変数を持つ）規則/等式の、
      ;; 自己比較ではない「別変数同士のアルファ同値」。
      ;;
      ;; 事実: vterm->fterm 分岐は dst 同士の mgu に src 対応 (rule1-src -> rule2-src) が
      ;; 含まれるかを member で検査する。src 変数が dst に出現しない場合、その mgu に
      ;; src 変数が現れないため member 検査が失敗し、本来 T であるべきところ NIL を返す。
      ;; fterm->vterm・等式版も同型の理由で取りこぼす。→ 下記 [A]-[D] は現実装で FAIL する。

      ;; --- 本来 T であるべき（別変数でのアルファ同値）---
      ;; [A] x->f(y) と z->f(w) は {x:=z, y:=w} で一致する同一規則。
      (is (alphabet-equivalent-p (rewrite-rule (vterm 'x) (fterm 'f (list (vterm 'y))))
                     (rewrite-rule (vterm 'z) (fterm 'f (list (vterm 'w))))))
      ;; [B] 対称性: 逆順も T であるべき。
      (is (alphabet-equivalent-p (rewrite-rule (vterm 'z) (fterm 'f (list (vterm 'w))))
                     (rewrite-rule (vterm 'x) (fterm 'f (list (vterm 'y))))))
      ;; [C] fterm->vterm 版 f(y)->x と f(w)->z。
      (is (alphabet-equivalent-p (rewrite-rule (fterm 'f (list (vterm 'y))) (vterm 'x))
                     (rewrite-rule (fterm 'f (list (vterm 'w))) (vterm 'z))))
      ;; [D] 等式版 x=f(y) と z=f(w)。
      (is (alphabet-equivalent-p (equation nil (vterm 'x) (fterm 'f (list (vterm 'y))))
                     (equation nil (vterm 'z) (fterm 'f (list (vterm 'w))))))

      ;; --- 過剰許容を防ぐ負ケース（現実装でも NIL。修正後も NIL を維持すべき）---
      ;; [E] x->f(x) は src 変数が dst に出現する形。z->f(w)（出現しない形）とは非同値。
      (is (not (alphabet-equivalent-p (rewrite-rule (vterm 'x) (fterm 'f (list (vterm 'x))))
                          (rewrite-rule (vterm 'z) (fterm 'f (list (vterm 'w)))))))
      ;; [F] dst の関数構造が異なれば非同値。
      (is (not (alphabet-equivalent-p (rewrite-rule (vterm 'x) (fterm 'f (list (vterm 'y))))
                          (rewrite-rule (vterm 'z) (fterm 'g (list (vterm 'w) (vterm 'w))))))))


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


(test clover.tests.unify.alphabet-equivalent-p.equation-set
      ;; clause-set 版(test3/test4)と対になる equation-set 版。これまで未カバーだった。
      ;; alphabet-equivalent-p の equation-set メソッドは set-difference を両方向に :test #'alphabet-equivalent-p で取る。
      ;; 変数リネームのみ異なる等式集合 → T。
      (is (alphabet-equivalent-p
            (equation-set (list (equation nil (vterm 'x) (constant 'A))
                                (equation nil (vterm 'x) (fterm 'f (list (vterm 'y))))))
            (equation-set (list (equation nil (vterm 'z) (constant 'A))
                                (equation nil (vterm 'z) (fterm 'f (list (vterm 'w))))))))
      ;; 片方の等式が非同値（A vs B）→ NIL。
      (is (not (alphabet-equivalent-p
                 (equation-set (list (equation nil (vterm 'x) (constant 'A))))
                 (equation-set (list (equation nil (vterm 'z) (constant 'B)))))))
      ;; 要素数が違えば → NIL。
      (is (not (alphabet-equivalent-p
                 (equation-set (list (equation nil (vterm 'x) (constant 'A))
                                     (equation nil (vterm 'x) (fterm 'f (list (vterm 'y))))))
                 (equation-set (list (equation nil (vterm 'z) (constant 'A)))))))
      ;; 空集合同士 → T。
      (is (alphabet-equivalent-p (equation-set nil) (equation-set nil))))


(test clover.tests.unify.subsumption-clause-p.boundary
      ;; 部分割り当ての早期枝刈り版 subsumption-clause-p の境界・退化ケース。
      ;; 空節(リテラル0個)の clause1 は任意の節を包摂する。
      ;; （長さ0で長さガードを通過し、割り当て対象が無いので空節に対する clause-subset が T。）
      (is (subsumption-clause-p
            (clause nil)
            (clause (list (literal nil 'P (list (constant 'A)))))))
      ;; 空節 vs 空節 も T。
      (is (subsumption-clause-p (clause nil) (clause nil)))
      ;; clause1 が clause2 より長ければ長さガードで NIL。
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P (list (vterm 'x)))
                               (literal nil 'Q (list (vterm 'y)))))
                 (clause (list (literal nil 'P (list (constant 'A))))))))
      ;; 重複リテラル {P(x),P(x)} は {P(A),P(B)} を包摂する（x:=A で {P(A),P(A)} ⊆ {P(A),P(B)}）。
      (is (subsumption-clause-p
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'P (list (vterm 'x)))))
            (clause (list (literal nil 'P (list (constant 'A)))
                          (literal nil 'P (list (constant 'B)))))))
      ;; 同長で順列を要する包摂 {P(x),Q(y)} vs {Q(B),P(A)} → T（探索が節の並び順を跨ぐ）。
      (is (subsumption-clause-p
            (clause (list (literal nil 'P (list (vterm 'x)))
                          (literal nil 'Q (list (vterm 'y)))))
            (clause (list (literal nil 'Q (list (constant 'B)))
                          (literal nil 'P (list (constant 'A)))))))
      ;; 変数共有が壊れる {P(x),Q(x)} vs {P(A),Q(B)} → NIL（x が A と B で矛盾）。
      (is (not (subsumption-clause-p
                 (clause (list (literal nil 'P (list (vterm 'x)))
                               (literal nil 'Q (list (vterm 'x)))))
                 (clause (list (literal nil 'P (list (constant 'A)))
                               (literal nil 'Q (list (constant 'B)))))))))


(test clover.tests.unify.find-most-general-unifier-set.disjoint-repeated-variable
      ;; 【変数素でも発生する単一化バグの再現 / RED / 確度: 高(再現済) / 現状 latent ではない】
      ;;
      ;; 事実: 2つのオペランドが変数を一切共有しない(＝standardize-apart 済み)場合でも、
      ;;   片方のリテラル内で同一変数が重複し、それが相手側の「同一関数記号を持つ2つの項」に
      ;;   対応すると、find-most-general-unifier-set は本来単一化可能なのに ununifiable を返す。
      ;;   例: P(x,x) と P(f(a),f(b))。x=f(a) かつ x=f(b) より f(a)=f(b) すなわち a=b で
      ;;   単一化可能(mgu 例 {x:=f(a), a:=b})。しかし現行実装は UNUNIFIABLE を返す。
      ;;   ※ x と a,b は別リテラル由来で共有していない(変数素)。それでも失敗する点が要点。
      ;;   対照: P(x,x) と P(f(w),g(v)) は f≠g で真に単一化不能(既存 test1 でカバー済)。
      ;;         P(x,x) と P(y,f(w)) は片側が変数なので可解(既存 test1 でカバー済)。
      ;; 根因(推測・確度中): disagreement-set が同一 src の非変数束縛 {x->f(a), x->f(b)} を
      ;;   持つとき、%flatten-disagreement-set(unify.lisp:107-138) が変数 x を相手 unifier の
      ;;   src に代入して src を fterm 化し、apply-unifier(substitute.lisp:77-81) が
      ;;   unexpected-unifier-source を送出、%find-most-general-unifier-set が ununifiable-error に
      ;;   変換する。本来は2つの dst f(a),f(b) を再帰的に単一化(→ a=b)する分解が必要。
      ;; 影響範囲(事実): 変数重複を含む項/リテラル(反射律 P(x,x)、規則 LHS f(x,x) 等)は一般的で、
      ;;   resolution(resolution.lisp:52,102)・rewrite(rewrite.lisp:50)・
      ;;   critical-pair(criticalpair.lisp:49,63)・subsumption(unify.lisp:263)のいずれも
      ;;   変数素化後の呼び出しで到達可能(＝latent ではなく現に取りこぼしを生む)。
      ;;
      ;; ununifiable-error は error 非継承(clover-toplevel-condition)なので自前で捕捉し、
      ;; クリーンな (is nil) 失敗に落とす。修正後は apply 後の両辺が等しくなり PASS。

      ;; D1: P(x,x) vs P(f(a),f(b))  ─ x はL1のみ, a,b はL2のみ(変数素) / a,b は変数
      (let ((l1 (literal nil 'P (list (vterm 'x) (vterm 'x))))
            (l2 (literal nil 'P (list (fterm 'f (list (vterm 'a)))
                                      (fterm 'f (list (vterm 'b)))))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil))))

      ;; D2: P(f(a),f(b)) vs P(y,y)  ─ D1 の対称形(変数素)
      (let ((l1 (literal nil 'P (list (fterm 'f (list (vterm 'a)))
                                      (fterm 'f (list (vterm 'b))))))
            (l2 (literal nil 'P (list (vterm 'y) (vterm 'y)))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil))))

      ;; 項レベル版(rewrite/critical-pair の呼び出しに相当): f(x,x) vs f(g(a),g(b))
      ;;   x=g(a) かつ x=g(b) より a=b で可解(mgu 例 {x:=g(a), a:=b})。
      (let ((t1 (fterm 'f (list (vterm 'x) (vterm 'x))))
            (t2 (fterm 'f (list (fterm 'g (list (vterm 'a)))
                                (fterm 'g (list (vterm 'b)))))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set t1 t2)))
                  (term=
                    (clover.substitute:apply-unifier-set t1 us)
                    (clover.substitute:apply-unifier-set t2 us)))
              (ununifiable-error () nil)))))
 


#|

REDケース。要修正。一旦コメントアウト。
変数素でないリテラル間のmguについても正しく計算する必要があり、それに関するテストである。
まずは、変数素の場合でも生じる不具合を修正したのち、こちらについても対応を行う。

|#
(skip-test
  (test clover.tests.unify.find-most-general-unifier-set.shared-swap-variables
      ;; 【監査で判明した単一化バグの再現 / RED / 確度: 高(再現済) / 現状は導出では latent】
      ;;
      ;; 事実: find-most-general-unifier-set は「変数を共有し位置が入れ替わった項」を
      ;;   誤って ununifiable と判定する。例: P(x,y) と P(y,x)。これらは mgu {x:=y} で
      ;;   単一化可能（両辺 P(y,y)）だが、現行実装は UNUNIFIABLE を返す。
      ;; 根因(推測・確度中): disagreement-set が相互ペア {x->y, y->x} を含むとき、
      ;;   %flatten-disagreement-set(unify.lisp:107-138) が y->x に x->y を適用して
      ;;   自己参照 y->y を作り、直後の occurrence-check で occurrence-check-error →
      ;;   %find-most-general-unifier-set が ununifiable-error に変換する。
      ;;   (変数素な f(x1,y1)/f(y2,x2) は相互ペアを生じないため正しく単一化される＝
      ;;    トリガーは「変数共有＋位置入替」)。
      ;; 影響範囲(事実): 導出パイプラインの mgu 呼び出し(resolution.lisp:52,102 と
      ;;   subsumption の unify.lisp:263)は常に standardize-apart 済みの変数素な引数で
      ;;   呼ぶため、本バグは現状 latent(対称述語定理の end-to-end 証明は成功する)。
      ;;   ただし find-most-general-unifier-set 単体の契約違反であり、変数共有項を
      ;;   直接単一化する将来のコード/リファクタで顕在化し得る landmine。
      ;;
      ;; ununifiable-error は error 非継承(clover-toplevel-condition)なので自前で捕捉し、
      ;; クリーンな (is nil) 失敗に落とす(修正後は literal= が真になり PASS)。
      (let ((l1 (literal nil 'P (list (vterm 'x) (vterm 'y))))
            (l2 (literal nil 'P (list (vterm 'y) (vterm 'x)))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil))))))


;; ─────────────────────────────────────────────────────────────────────────
;; 症状2(変数共有): 単一化が停止しない
;;
;; 事実: P(y,f(A)) と P(y,y) は mgu {y:=f(A)} で単一化可能(両辺 P(f(A),f(A)))だが、
;;   現行実装は停止しない。y を両リテラルで共有している(＝変数素でない入力)。
;; 根因(推測・確度中): disagreement-set が恒等単一化子 y->y を含み(両リテラルが変数 y を
;;   同位置に共有するため)、%select-one-of-substitutable-unifier が y->y を毎回「代入可能」
;;   と判定するが、y->y による flatten は集合を変化させないため、
;;   %find-most-general-unifier-set のループ(unify.lisp:147-149)が不動点に到達せず回り続ける。
;;   本来は恒等単一化子を除去すべき(disjoint 版 D7 が停止する事実と整合)。
(test clover.tests.unify.find-most-general-unifier-set.shared-nonterminating1
      (let ((l1 (literal nil 'P (list (vterm 'y) (fterm 'f (list (constant 'A))))))
            (l2 (literal nil 'P (list (vterm 'y) (vterm 'y)))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil)))))

(test clover.tests.unify.find-most-general-unifier-set.shared-nonterminating2
      (let ((l1 (literal nil 'P (list (vterm 'y) (fterm 'f (list (constant 'A))))))
            (l2 (literal nil 'P (list (vterm 'z) (vterm 'z)))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil)))))


;; ─────────────────────────────────────────────────────────────────────────
;; 症状3(変数共有): 位置が交差した関数項を誤って ununifiable と判定
;; 事実: P(f(x),y) と P(y,f(B)) は f(x)=y かつ y=f(B) より x=B で単一化可能
;;   (mgu {y:=f(B), x:=B})。しかし現行実装は UNUNIFIABLE を返す。
;;   y を両リテラルで共有している(＝変数素でない入力)。
;; 根因(推測・確度中): disagreement-set {y->f(x), y->f(B)} に対し flatten が変数 y を
;;   相手 unifier の src に代入して src を fterm 化 → unexpected-unifier-source →
;;   ununifiable-error。本来は2つの dst f(x),f(B) を再帰単一化(→ x=B)すべき。
;;   disjoint-repeated-variable と同型の「分解欠落」だが、こちらは変数共有でも発火する。
(test clover.tests.unify.find-most-general-unifier-set.shared-crossed-function-term1
      (let ((l1 (literal nil 'P (list (fterm 'f (list (vterm 'x))) (vterm 'y))))
            (l2 (literal nil 'P (list (vterm 'y) (fterm 'f (list (constant 'B)))))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil)))))

(test clover.tests.unify.find-most-general-unifier-set.shared-crossed-function-term2
      (let ((l1 (literal nil 'P (list (fterm 'f (list (vterm 'x))) (vterm 'y))))
            (l2 (literal nil 'P (list (vterm 'z) (fterm 'f (list (constant 'B)))))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set l1 l2)))
                  (clover.equality:literal=
                    (clover.substitute:apply-unifier-set l1 us)
                    (clover.substitute:apply-unifier-set l2 us)))
              (ununifiable-error () nil)))))



(test clover.tests.unify.find-most-general-unifier-set.disjoint-merge-identity-nonterminating
#|
【merge が生む恒等単一化子による非停止 / 変数素でも発火 / 確度:高(最小再現済) / 現状:非停止】


事実(最小再現): find-most-general-unifier-set は次を停止できない。
    f(a,a) と f(g(x,y),g(z,y))            ← 停止しない
  対して、分解される右辺が変数を共有しないと停止する(対照):
    f(a,a) と f(g(x,y),g(z,w)) => {a:=g(z,w), x:=z, y:=w}   ← 停止する
  ※ a と x,y,z は別オペランド由来で共有なし(変数素)。それでも非停止になる点が要点。

根因(pinpoint済・確度高): disjoint-repeated-variable と同じ merge 経路で、同一 src の2束縛
  {a->g(x,y), a->g(z,y)} の右辺 g(x,y) と g(z,y) を %collect-disagreement-set が再帰分解する。
  両者が変数 y を共有するため、分解が恒等単一化子 y->y を生む。恒等 y->y は a->g(x,y) の dst に
  y が出現するため %select-one-of-substitutable-unifier(unify.lisp:91-104) に毎回選ばれ続け、
  %find-most-general-unifier-set の flatten ループ(unify.lisp:147-149)が不動点に到達せず無限ループ。
  (= step2 の症状2「恒等代入による非停止」を merge が内部で再誘発している。)

推奨修正: %collect-disagreement-set が結果を返す前に (term= src dst) な恒等 unifier を除去する
  (恒等は制約なしで健全性に無影響)。これで本ケースが停止し、step2 の症状1・2 も同時に解消する。

影響(事実): この非停止は completion の内側(all-critical-pair→alphabet-equivalent-p→subsumption→
  find-most-general-unifier-set)で踏まれ、grp012_4 等の完備化を失敗させる(変数共有する部分項は
  群論/等式推論で頻出)。旧unifyは不完全でこの入力に到達しなかったため露見していなかった。

修正後は apply 後の両辺が等しくなり PASS(f(a,a),f(g(x,y),g(z,y)) は a:=g(z,y),x:=z 等で可解)。
|# 
      (let ((t1 (fterm 'f (list (vterm 'a) (vterm 'a))))
            (t2 (fterm 'f (list (fterm 'g (list (vterm 'x) (vterm 'y)))
                                (fterm 'g (list (vterm 'z) (vterm 'y)))))))
        (is (handler-case
                (let ((us (find-most-general-unifier-set t1 t2)))
                  (term=
                    (clover.substitute:apply-unifier-set t1 us)
                    (clover.substitute:apply-unifier-set t2 us)))
              (ununifiable-error () nil)))))

