(defpackage clover.tests.substitute
  (:use :cl
        :clover.conditions
        :clover.types
        :clover.util
        :clover.substitute
        :1am))
(in-package :clover.tests.substitute)

;; 横展開調査で判明したカバレッジ欠落の補填。
;; これまで tests/src/substitute.lisp はパッケージ定義のみで test が1件も無かった
;; （.asd には登録済み）。apply-unifier / apply-unifier-set の正常系を固定する。

(test clover.tests.substitute.apply-unifier.term
      ;; 変数への適用
      (is (term= (apply-unifier (vterm 'x) (unifier (vterm 'x) (constant 'A)))
                 (constant 'A)))
      ;; 対象でない変数は不変
      (is (term= (apply-unifier (vterm 'y) (unifier (vterm 'x) (constant 'A)))
                 (vterm 'y)))
      ;; 定数は不変、かつ constant 型が保たれる
      (let ((r (apply-unifier (constant 'B) (unifier (vterm 'x) (constant 'A)))))
        (is (term= r (constant 'B)))
        (is (typep r 'constant)))
      ;; fterm の引数位置に適用
      (is (term= (apply-unifier (fterm 'f (list (vterm 'x) (constant 'C)))
                                (unifier (vterm 'x) (constant 'A)))
                 (fterm 'f (list (constant 'A) (constant 'C)))))
      ;; ネストした出現すべてに適用
      (is (term= (apply-unifier
                   (fterm 'f (list (vterm 'x) (fterm 'g (list (vterm 'x)))))
                   (unifier (vterm 'x) (fterm 'h (list (vterm 'y)))))
                 (fterm 'f (list (fterm 'h (list (vterm 'y)))
                                 (fterm 'g (list (fterm 'h (list (vterm 'y))))))))))

(test clover.tests.substitute.apply-unifier.literal-clause
      ;; リテラルへの適用（述語・符号を保持）
      (is (literal= (apply-unifier (literal t 'P (list (vterm 'x) (vterm 'y)))
                                   (unifier (vterm 'x) (constant 'A)))
                    (literal t 'P (list (constant 'A) (vterm 'y)))))
      ;; 命題リテラル（空引数）は不変
      (is (literal= (apply-unifier (literal nil 'P nil)
                                   (unifier (vterm 'x) (constant 'A)))
                    (literal nil 'P nil)))
      ;; 節への適用
      (is (clause= (apply-unifier
                     (clause (list (literal nil 'P (list (vterm 'x)))
                                   (literal t 'Q (list (vterm 'x) (vterm 'y)))))
                     (unifier (vterm 'x) (constant 'A)))
                   (clause (list (literal nil 'P (list (constant 'A)))
                                 (literal t 'Q (list (constant 'A) (vterm 'y))))))))

(test clover.tests.substitute.apply-unifier-set
      ;; 複数の単一化子をまとめて適用
      (is (term= (apply-unifier-set
                   (fterm 'f (list (vterm 'x) (vterm 'y)))
                   (unifier-set (list (unifier (vterm 'x) (constant 'A))
                                      (unifier (vterm 'y) (constant 'B)))))
                 (fterm 'f (list (constant 'A) (constant 'B)))))
      ;; 空の単一化子集合 → 項は不変
      (is (term= (apply-unifier-set (fterm 'f (list (vterm 'x)))
                                    (unifier-set nil))
                 (fterm 'f (list (vterm 'x)))))
      ;; 等式への適用（型・符号を保持）
      (is (equation= (apply-unifier-set
                       (equation nil (vterm 'x) (fterm 'g (list (vterm 'x))))
                       (unifier-set (list (unifier (vterm 'x) (constant 'A)))))
                     (equation nil (constant 'A) (fterm 'g (list (constant 'A)))))))
