;;;; prove 関数(統一証明インターフェース)の受け入れテスト
;;;;
;;;; 検証する仕様:
;;;;  - target は「証明したい式そのもの(肯定形)」。clause の場合は
;;;;    「リテラルの連言(自由変数は存在量化)」として解釈される(types.lisp 参照)。
;;;;  - 戦略選択: 等式ターゲット×(rrs または等式理論) → :rewriting、
;;;;    節ターゲット×(節集合 or 前提なし) → :resolution、その他 → :unknown。
;;;;  - status: :proved / :disproved(書き換え経路のみ。完備化済み系は決定手続き) /
;;;;    :unknown(探索し尽くし・完備化失敗・未対応入力) / :timeout。
;;;;  - タイムアウト: 残余時間方式。完備化・導出とも制限時間で打ち切られ
;;;;    :timeout の prove-result が返る(条件は外に漏れない)。
;;;;  - completed-rewrite-rule-set: prove 内部で完備化した場合は結果に返却され、
;;;;    呼び出し側がキャッシュして次回 input に渡せる。

(defpackage clover.tests.prove
  (:use :cl
        :clover.types
        :1am)
  (:import-from :clover.clover
                :prove)
  (:import-from :clover.parser
                :parse-mkbtt-expression))
(in-package :clover.tests.prove)


;;; ---- 素材: ペアノ算術(等式系) ----

(defun %s (a) (fterm 's (list a)))
(defun %p (a b) (fterm 'plus (list a b)))
(defparameter *zero* (constant 'zero))

(defparameter *peano-eqs*
  ;; plus(x,0) = x / plus(x,s(y)) = s(plus(x,y))
  (equation-set
    (list (equation nil (%p (vterm 'x) *zero*) (vterm 'x))
          (equation nil (%p (vterm 'x) (%s (vterm 'y)))
                        (%s (%p (vterm 'x) (vterm 'y)))))))

(defun %peano-clause-premises ()
  ;; 同じ公理を「単位等式節の節集合」として表現したもの(変換経路の検証用)
  (clause-set
    (mapcar (lambda (e) (clause (list e)))
            (equation-set.equations *peano-eqs*))))

(defun %one+one=two ()
  (equation nil (%p (%s *zero*) (%s *zero*)) (%s (%s *zero*))))

;;; ---- 素材: 家系図(節集合) ----

(defparameter *family-premises*
  (let ((x (vterm 'x)) (y (vterm 'y)) (z (vterm 'z)))
    (clause-set
      (list
        (clause (list (literal nil 'parent (list (constant 'tom) (constant 'bob)))))
        (clause (list (literal nil 'parent (list (constant 'bob) (constant 'ann)))))
        (clause (list (literal nil 'parent (list (constant 'ann) (constant 'pat)))))
        ;; ancestor(x,y) :- parent(x,y)
        (clause (list (literal t 'parent (list x y))
                      (literal nil 'ancestor (list x y))))
        ;; ancestor(x,z) :- parent(x,y), ancestor(y,z)
        (clause (list (literal t 'parent (list x y))
                      (literal t 'ancestor (list y z))
                      (literal nil 'ancestor (list x z))))))))


;;; ======================================================================
;;; 書き換え経路 (:rewriting)
;;; ======================================================================

(test clover.tests.prove.rewriting.internal-completion-proved
      ;; 式集合前提・rrs なし → prove が内部で完備化し、真の等式を :proved。
      ;; 生成した規則集合が結果に返却される(キャッシュ用)。
      (let ((result (prove (prove-input (%one+one=two) *peano-eqs* nil nil nil))))
        (is (eq :proved (prove-result.prove-status result)))
        (is (eq :rewriting (prove-result.prove-engine result)))
        (is (typep (prove-result.completed-rewrite-rule-set result)
                   'rewrite-rule-set))
        ;; supporting-info は各リテラルの正規形(equation)のリスト
        (is (every (lambda (e) (typep e 'equation))
                   (prove-result.supporting-info result)))
        (is (typep (prove-result.elapsed-seconds result) '(real 0)))))

(test clover.tests.prove.rewriting.rrs-cache-round-trip
      ;; 1回目の prove が返した rrs を2回目の input に渡す(行1経路)。
      ;; 結果の rrs は入力と同一オブジェクトであること。
      (let* ((first-result (prove (prove-input (%one+one=two) *peano-eqs* nil nil nil)))
             (rrs (prove-result.completed-rewrite-rule-set first-result))
             (second-result (prove (prove-input (%one+one=two) nil rrs nil nil))))
        (is (eq :proved (prove-result.prove-status second-result)))
        (is (eq :rewriting (prove-result.prove-engine second-result)))
        (is (eq rrs (prove-result.completed-rewrite-rule-set second-result)))))

(test clover.tests.prove.rewriting.disproved
      ;; 偽の等式 0+0=1 は決定手続きとして :disproved になる
      (let ((result (prove (prove-input
                             (equation nil (%p *zero* *zero*) (%s *zero*))
                             *peano-eqs* nil nil nil))))
        (is (eq :disproved (prove-result.prove-status result)))
        (is (eq :rewriting (prove-result.prove-engine result)))))

(test clover.tests.prove.rewriting.negated-equation-proved
      ;; 真の不等式 0+0≠1 (negation=t を肯定形の主張として与える) は :proved
      (let ((result (prove (prove-input
                             (equation t (%p *zero* *zero*) (%s *zero*))
                             *peano-eqs* nil nil nil))))
        (is (eq :proved (prove-result.prove-status result)))))

(test clover.tests.prove.rewriting.clause-set-premises-conversion
      ;; 前提が「単位等式節の節集合」でも等式集合へ変換されて書き換え経路に乗る
      (let ((result (prove (prove-input
                             (%one+one=two) (%peano-clause-premises) nil nil nil))))
        (is (eq :proved (prove-result.prove-status result)))
        (is (eq :rewriting (prove-result.prove-engine result)))))

(test clover.tests.prove.rewriting.conjunctive-target
      ;; 等式の連言ターゲット: 全リテラル成立で :proved、1つでも不成立なら :disproved
      (let ((true-conj
              (clause (list (equation nil (%p (%s *zero*) *zero*) (%s *zero*))
                            (equation nil (%p *zero* (%s *zero*)) (%s *zero*)))))
            (half-false-conj
              (clause (list (equation nil (%p (%s *zero*) *zero*) (%s *zero*))
                            (equation nil (%p *zero* *zero*) (%s *zero*))))))
        (is (eq :proved
                (prove-result.prove-status
                  (prove (prove-input true-conj *peano-eqs* nil nil nil)))))
        (is (eq :disproved
                (prove-result.prove-status
                  (prove (prove-input half-false-conj *peano-eqs* nil nil nil)))))))

(test clover.tests.prove.rewriting.completion-failure-unknown
      ;; 向き付け不能な等式系(可換律のみ)は完備化が失敗し :unknown になる。
      ;; supporting-info に理由 :completion-failed が入る。
      (let* ((commutativity
               (equation-set
                 (list (equation nil
                                 (fterm 'f (list (vterm 'x) (vterm 'y)))
                                 (fterm 'f (list (vterm 'y) (vterm 'x)))))))
             (result (prove (prove-input
                              (equation nil
                                        (fterm 'f (list *zero* (%s *zero*)))
                                        (fterm 'f (list (%s *zero*) *zero*)))
                              commutativity nil nil nil))))
        (is (eq :unknown (prove-result.prove-status result)))
        (is (member :completion-failed (prove-result.supporting-info result)))))


;;; ======================================================================
;;; 導出経路 (:resolution)
;;; ======================================================================

(test clover.tests.prove.resolution.proved
      ;; ancestor(tom,pat): 3世代の推移閉包。target は肯定形で与える。
      ;; supporting-info は反駁の最終ノード(clause-set)。
      (let ((result (prove (prove-input
                             (clause (list (literal nil 'ancestor
                                                    (list (constant 'tom)
                                                          (constant 'pat)))))
                             *family-premises* nil nil nil))))
        (is (eq :proved (prove-result.prove-status result)))
        (is (eq :resolution (prove-result.prove-engine result)))
        (is (typep (prove-result.supporting-info result) 'clause-set))))

(test clover.tests.prove.resolution.variable-query-proved
      ;; 変数付きターゲット ancestor(tom, w) は存在証明として成功する
      (let ((result (prove (prove-input
                             (clause (list (literal nil 'ancestor
                                                    (list (constant 'tom)
                                                          (vterm 'w)))))
                             *family-premises* nil nil nil))))
        (is (eq :proved (prove-result.prove-status result)))))

(test clover.tests.prove.resolution.conjunctive-target-proved
      ;; target 節は「連言」として解釈される: P(A)∧Q(A) を {P(A)},{Q(A)} から証明。
      ;; (選言解釈なら前提1つで足りてしまうため、両前提が必要なこの形で連言解釈を固定する)
      (let ((premises (clause-set
                        (list (clause (list (literal nil 'p (list (constant 'a)))))
                              (clause (list (literal nil 'q (list (constant 'a))))))))
            (target (clause (list (literal nil 'p (list (constant 'a)))
                                  (literal nil 'q (list (constant 'a)))))))
        (is (eq :proved
                (prove-result.prove-status
                  (prove (prove-input target premises nil nil nil)))))))

(test clover.tests.prove.resolution.no-premises-unknown
      ;; 前提なしの原子論理式は証明できず、有限の探索で :unknown が返り停止する
      (let ((result (prove (prove-input
                             (clause (list (literal nil 'q (list (constant 'a)))))
                             nil nil nil nil))))
        (is (eq :unknown (prove-result.prove-status result)))
        (is (eq :resolution (prove-result.prove-engine result)))))

(test clover.tests.prove.unknown.bare-equation-without-usable-premises
      ;; 裸の equation ターゲットで書き換え経路が使えない場合は :unknown 直行
      ;; (導出フォールバックはしない、という設計判断の固定)
      (let ((result (prove (prove-input
                             (equation nil *zero* (%s *zero*))
                             nil nil nil nil))))
        (is (eq :unknown (prove-result.prove-status result)))
        (is (eq :none (prove-result.prove-engine result)))))


;;; ======================================================================
;;; タイムアウト (:timeout)
;;; ======================================================================

(test clover.tests.prove.timeout.resolution
      ;; 再帰規則により停止しない問い合わせ ancestor(pat,tom) が
      ;; timeout で :timeout として打ち切られる(タイムアウトなしでは無限走行する入力)
      (let ((result (prove (prove-input
                             (clause (list (literal nil 'ancestor
                                                    (list (constant 'pat)
                                                          (constant 'tom)))))
                             *family-premises* nil 1 nil))))
        (is (eq :timeout (prove-result.prove-status result)))
        (is (eq :resolution (prove-result.prove-engine result)))
        ;; elapsed は実測値(概ね timeout 近辺)
        (is (<= 0.9 (prove-result.elapsed-seconds result) 30))))

(test clover.tests.prove.timeout.completion
      ;; 完備化が終わらない難問(AD93_Z22: 外部ツールも全て時間切れの系)で
      ;; 完備化フェーズが打ち切られ :timeout になる
      (let* ((hard (parse-mkbtt-expression
                     (alexandria:read-file-into-string
                       (asdf:system-relative-pathname
                         :clover-test
                         "tests/resources/eq_systems_hard/AD93_Z22.trs"))))
             (eq1 (first (equation-set.equations hard)))
             (result (prove (prove-input
                              (equation nil (equation.left eq1) (equation.right eq1))
                              hard nil 2 nil))))
        (is (eq :timeout (prove-result.prove-status result)))
        (is (eq :rewriting (prove-result.prove-engine result)))))

(test clover.tests.prove.timeout.not-triggered-when-fast
      ;; 十分な時間があれば timeout 指定があっても正常に :proved で返る
      (let ((result (prove (prove-input (%one+one=two) *peano-eqs* nil 30 nil))))
        (is (eq :proved (prove-result.prove-status result)))
        (is (< (prove-result.elapsed-seconds result) 30))))
