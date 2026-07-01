(defpackage clover.tests.unify
  (:use :cl
        :clover.conditions
        :clover.unify
        :clover.types
        :clover.logical-predicates
        :1am)
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

