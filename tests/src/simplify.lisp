(defpackage clover.tests.simplify
  (:use :cl
        :clover.types
        :clover.logical-predicates
        :clover.unify
        :clover.simplify
        :1am)
  (:import-from :clover.equality
                :clause=
                :clause-set=))
(in-package :clover.tests.simplify)



(test clover.tests.simplify.%include-law-of-exclude-middle-p

      (is (clover.simplify::%include-law-of-exclude-middle-p
            (clause (list (literal nil 'P (list (fterm 'g (list (vterm 'x)))))
                          (literal t 'P   (list (fterm 'g (list (vterm 'x)))))))))

      (is (not (clover.simplify::%include-law-of-exclude-middle-p
                 (clause (list (literal nil 'P (list (fterm 'g (list (vterm 'x)))
                                                     (fterm 'h (list (vterm 'x)))))
                               (literal t 'P   (list (fterm 'g (list (vterm 'x))))))))))

      (is (not (clover.simplify::%include-law-of-exclude-middle-p
                 (clause (list 
                           (literal nil 'P   (list (fterm 'g (list (vterm 'x)))))
                           (literal t 'P (list (fterm 'g (list (vterm 'x)))
                                                 (fterm 'h (list (vterm 'x))))))))))

      )


(test clover.tests.simplify.%remove-independent-clause

      (is 
        (let*((clause1
                (clause (list (literal nil 'P (list (constant 'A )))
                              (literal nil 'P (list (fterm 'g (list (vterm 'x))))))))
              (clause2
                (clause (list (literal nil 'P (list (vterm 'x)))
                              (literal nil 'P (list (constant 'h ))))))
              (clauses
                (list clause1 clause2))
              (expected
                (clause-set nil)))
          (clause-set= (clause-set (clover.simplify::%remove-independent-clause clauses))
                       expected)))
      
      (is 
        (let*((clause1
                (clause (list (literal t 'P (list (constant 'A )))
                              (literal nil 'P (list (fterm 'g (list (vterm 'x))))))))
              (clause2
                (clause (list (literal nil 'Q (list (vterm 'x)))
                              (literal nil 'P (list (constant 'h ))))))
              (clauses
                (list clause1 clause2))
              (expected
                (clause-set (list 
                              (clause (list (literal t 'P (list (constant 'A )))
                                            (literal nil 'P (list (fterm 'g (list (vterm 'x)))))))))))
          (clause-set= (clause-set (clover.simplify::%remove-independent-clause clauses))
                       expected)))
      
      (is 
        (let*((clause1
                (clause (list (literal t 'P (list (constant 'A )))
                              (literal t 'P (list (fterm 'g (list (vterm 'x))))))))
              (clause2
                (clause (list (literal t 'P (list (vterm 'x)))
                              (literal t 'P (list (constant 'h ))))))
              (clauses
                (list clause1 clause2))
              (expected
                (clause-set nil)))
          (clause-set= (clause-set (clover.simplify::%remove-independent-clause clauses))
                       expected)))
      )


(test clover.tests.simplify.simplify

      (is 
        (let*((clause1
                (clause (list (literal t 'P (list (constant 'f )))
                              (literal nil 'P (list (constant 'f ))))))
              (clause2
                (clause (list (literal t 'P (list (vterm 'x)))
                              (literal nil 'P (list (vterm 'x))))))
              (clause-set
                (clause-set (list clause1 clause2)))
              (expected
                (clause-set nil )))
          (clause-set= (simplify clause-set) expected)))

      )


(test clover.tests.simplify.%remove-subsumption
      ;; %remove-subsumption は「後続(j>i)の節が target を包摂するとき target を削除」する。
      ;; ここでは現実装で正しく動く（削除が起きる）ケースを固定する。
      (let ((general (clause (list (literal nil 'P (list (vterm 'x))))))    ; P(x)（一般）
            (special (clause (list (literal nil 'P (list (constant 'A))))))) ; P(A)（特殊）
        ;; (特殊, 一般): 後続の一般 P(x) が前の特殊 P(A) を包摂 → 特殊が削除され1件、一般が残る。
        (let ((result (clover.simplify::%remove-subsumption (list special general))))
          (is (= 1 (length result)))
          (is (clause= (first result) general)))
        ;; 互いに包摂しない節同士はどちらも残る。
        (let ((result (clover.simplify::%remove-subsumption
                        (list (clause (list (literal nil 'P (list (constant 'A)))))
                              (clause (list (literal nil 'Q (list (constant 'B)))))))))
          (is (= 2 (length result))))))


(test clover.tests.simplify.%remove-subsumption.order-dependence
      ;; 不具合の実証（現実装では FAIL する）:
      ;; %remove-subsumption の走査は j>i（後方限定）かつ包摂は非対称（一般 P(x) が
      ;; 特殊 P(A) を包摂し、逆は成り立たない）。このため「一般節が特殊節より前に並ぶ」と
      ;; 後方に包摂者が居らず、冗長な特殊節 P(A) が削除されない（並び順依存の削除漏れ）。
      ;; 包摂による冗長節除去は本来並び順に依存すべきでないため、両順序で特殊節は削除され
      ;; 結果1件になるべき。健全性は損なわれない（冗長節が残るだけ）が、削除取りこぼしは
      ;; 探索空間の肥大につながり得る。
      ;; 事実: src/simplify.lisp の %remove-subsumption が内側ループで (> j i) のみを見る。
      ;; 推奨(確度高): 「ある j(≠i) の節が target を包摂し、かつ相互包摂(=同値)でない、
      ;;   または同値なら index で一意の代表のみ残す」形にすれば、非対称包摂を順序に依らず削除できる。
      (let ((general (clause (list (literal nil 'P (list (vterm 'x))))))
            (special (clause (list (literal nil 'P (list (constant 'A)))))))
        ;; (一般, 特殊): 特殊 P(A) は一般 P(x) に包摂されるので削除され1件になるべき。
        ;; 現実装は2件を返すため、この is は FAIL する。
        (let ((result (clover.simplify::%remove-subsumption (list general special))))
          (is (= 1 (length result)))
          (is (clause= (first result) general)))))


(test clover.tests.simplify.%remove-alphabet-equal-clause
      ;; 変数名のみ異なる同値節の重複除去。alphabet-equivalent-p は対称なので、各重複は必ず後続に
      ;; 同値節を持って削除され、最後の1つだけが残る（%remove-subsumption と違い順序非依存）。
      (let ((c1 (clause (list (literal nil 'P (list (vterm 'x))))))
            (c2 (clause (list (literal nil 'P (list (vterm 'y))))))
            (c3 (clause (list (literal nil 'P (list (vterm 'z)))))))
        ;; 同値2節 → 1件。
        (is (= 1 (length (clover.simplify::%remove-alphabet-equal-clause (list c1 c2)))))
        ;; 同値3節 → 1件。
        (is (= 1 (length (clover.simplify::%remove-alphabet-equal-clause (list c1 c2 c3)))))
        ;; 非同値（P(x) と Q(x)）は両方残る。
        (is (= 2 (length (clover.simplify::%remove-alphabet-equal-clause
                           (list c1 (clause (list (literal nil 'Q (list (vterm 'x))))))))))))


(test clover.tests.simplify.%remove-subsumption.mutual-subsumption
      ;; 回帰ガード: 相互に包摂し合う節（アルファ同値の変種・完全な重複節）は論理的に等価で
      ;; あり、代表を1つ残すべき。素朴な「自分以外のどれかが包摂するなら削除」だと、変種ペアは
      ;; 互いに相手に包摂されるため両方とも削除され、節が丸ごと失われる（完全性の毀損）。
      ;; 現行の %remove-subsumption は strict subsumption（逆向き不成立）なら削除し、
      ;; 相互包摂なら index（< j i）で最前の代表のみ残すため、変種・重複は 1 件に集約される。
      ;; （この回帰が再発すると 0 件になり、下記 (= 1 ...) が FAIL する。）

      ;; 変種 {P(x)} と {P(y)}（アルファ同値）→ 代表1件が残る（両方消えてはいけない）
      (is (= 1 (length
                 (clover.simplify::%remove-subsumption
                   (list (clause (list (literal nil 'P (list (vterm 'x)))))
                         (clause (list (literal nil 'P (list (vterm 'y))))))))))
      ;; 完全な重複節 {P(x)} と {P(x)}（別オブジェクト・同値）→ 代表1件が残る
      (is (= 1 (length
                 (clover.simplify::%remove-subsumption
                   (list (clause (list (literal nil 'P (list (vterm 'x)))))
                         (clause (list (literal nil 'P (list (vterm 'x))))))))))

      ;; 参考: strict subsumption（一般が特殊を包摂し逆は不成立）は方針に依らず
      ;; 冗長な特殊節が消えて 1 件になるべき（順序非依存）。これは正しい修正で PASS する。
      (is (= 1 (length
                 (clover.simplify::%remove-subsumption
                   (list (clause (list (literal nil 'P (list (vterm 'x)))))
                         (clause (list (literal nil 'P (list (constant 'A))))))))))
      (is (= 1 (length
                 (clover.simplify::%remove-subsumption
                   (list (clause (list (literal nil 'P (list (constant 'A)))))
                         (clause (list (literal nil 'P (list (vterm 'x)))))))))))
