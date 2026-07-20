(defpackage clover.tests.resolution
  (:use :cl
        :clover.types
        :clover.logical-predicates
        :clover.unify
        :clover.resolution
        :1am)
  (:import-from :clover.tests.util
                :skip-test
                )
  (:import-from :clover.equality
                :clause=
                :clause-set=)
  (:import-from :clover.clover
                :start_resolution))
(in-package :clover.tests.resolution)


;;;; ==========================================================================
;;;; 監査(導出コア)で判明した「完全性ギャップ」の再現テスト
;;;;
;;;; 以下2件はいずれも RED(現行実装では FAIL する)。src/ は未修正であり、
;;;; テストを通すために実装を書き換えてはならない(役割の原則)。実装(人間側)で
;;;; 該当ギャップを解消したとき GREEN になる、という「あるべき仕様」の固定である。
;;;; ==========================================================================

(skip-test
  (test clover.tests.resolution.factoring-incompleteness
      ;; 【factoring 不在による不完全性 / 確度: 高 / 経験的に再現済み】
      ;; 事実: src/ 全体に factoring 規則が存在しない(grep 0 件)。:default 導出
      ;;   (resolution.lisp:27-80)は「解消リテラルと literal=/complement な複製の除去」
      ;;   は行うが、同符号リテラル2本を mgu で1本に併合する本来の factoring を行わない。
      ;; 反例(併せて充足不能):
      ;;   base   : {P(x), P(y)}     (≡ ∀x P(x))
      ;;   conseq : {!P(u), !P(v)}   (≡ ∀u !P(u))
      ;; 二項導出のみでは 2 リテラル節を生み続け □ に到達しない。A* は有限(重複排除済)
      ;;   状態を探索し尽くし foundp=NIL を返す(タイムアウトではない)。
      ;; factoring 追加後は foundp=T となるべき。現状この is は FAIL する。
      (multiple-value-bind (foundp node)
          (start_resolution
            (clause-set
              (list
                (clause (list (literal nil 'P (list (vterm 'x)))
                              (literal nil 'P (list (vterm 'y)))))
                (clause (list (literal t 'P (list (vterm 'u)))
                              (literal t 'P (list (vterm 'v))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp))))


#|

前提の無矛盾を前提とする

(test clover.tests.resolution.set-of-support-incompleteness
      ;; 【頂節=conseq 固定(set-of-support/線形)による不完全性 / 確度: 事実(再現済) /
      ;;   実害の有無は設計意図次第】
      ;; 事実: prepare-resolution(clover.lisp:84-114)は conseq を唯一の :center とし、
      ;;   :default opener(resolution.lisp:182-202)は :center のみを他節と導出する。
      ;;   このため反駁が conseq を経由しない場合(例: 前提節だけで不整合)、□ に到達しない。
      ;; 反例(全体は充足不能ゆえ妥当な含意):
      ;;   premises: {P}, {!P}   ← これだけで不整合
      ;;   conseq  : {!Q}        ← 反駁に不要な無関係ゴール
      ;; center=!Q は何とも導出できず foundp=NIL。
      ;; 注: 多くの証明器は「前提の無矛盾」を仮定するため、これを許容する設計もあり得る。
      ;;   その場合は本テストを「前提無矛盾を仮定するため対象外」として削除/無効化してよい。
      (multiple-value-bind (foundp node)
          (start_resolution
            (clause-set
              (list
                (clause (list (literal nil 'P nil)))
                (clause (list (literal t 'P nil)))
                (clause (list (literal t 'Q nil)) nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))
|#



;;;; ==========================================================================
;;;; factoring (節レベル) の仕様固定テスト
;;;;
;;;; 対象: (clover.resolution::factoring <clause> <resolution-mode>)
;;;;   戻り値は「因子の *リスト*」。0個・1個・複数個いずれもあり得る。
;;;;   親節そのものは含めない(親の保持は呼び出し側=節集合レベルの責務)。
;;;;
;;;; 現行の雛形実装(resolution.lisp:148-158)は clause をそのまま返すため、
;;;; 以下はすべて RED である。テストを通すために src/ を書き換えてはならない。
;;;; ==========================================================================

(defun %factor-list= (actual expected)
  "因子リストの一致判定。順序は無視し、変数名の違いは alphabet-equivalent-p で吸収する。
   actual がリストでない(= 雛形のように clause を直接返す)場合は不一致とする。"
  (and (listp actual)
       (= (length actual) (length expected))
       (every (lambda (e) (member e actual   :test #'alphabet-equivalent-p)) expected)
       (every (lambda (a) (member a expected :test #'alphabet-equivalent-p)) actual)))


(test clover.tests.resolution.factoring.returns-list
      ;; 因子が存在しない節に対しては、空リスト(NIL)を返すこと。
      ;; 単一リテラル節・同符号だが単一化不能・述語が異なる、のいずれも因子なし。
      ;; 「因子なし」は例外ケースではなく通常経路である(大半の節がこれに該当する)。
      (is (null (clover.resolution::factoring
                  (clause (list (literal nil 'P (list (vterm 'x)))))
                  :default)))
      (is (null (clover.resolution::factoring
                  (clause (list (literal nil 'P (list (constant 'A)))
                                (literal nil 'P (list (constant 'B)))))
                  :default)))
      (is (null (clover.resolution::factoring
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal nil 'Q (list (vterm 'y)))))
                  :default))))


(test clover.tests.resolution.factoring.self-pair-must-be-excluded
      ;; 【自己ペア除外 / 実測済み】
      ;; 単一化の修正(恒等 unifier の除去)により mgu(L,L) は「空の unifier-set」を
      ;; 正常に返す(エラーにならない)。同一リテラル対や literal= な重複リテラル対を
      ;; factoring の対象に含めると、恒等代入により *親と同一の節* が因子として得られ、
      ;; 後継ノードが親と同型になって探索が空回りする。
      ;; 走査は i<j の対に限ること。
      ;; (1) 単一化可能な対を持たない節は、自己ペアを含めると親そのものが返る。
      (let ((factors (clover.resolution::factoring
                       (clause (list (literal nil 'P (list (constant 'A)))
                                     (literal nil 'Q (list (constant 'B)))))
                       :default)))
        (is (null factors)))
      ;; (2) 因子を持つ節でも、自己ペアを含めると親のコピーが余分に混ざる。
      ;;     {P(x), P(a), Q(x)} の真の因子は1つだけである。
      (let* ((parent  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (constant 'A)))
                                    (literal nil 'Q (list (vterm 'x))))))
             (factors (clover.resolution::factoring parent :default)))
        (is (listp factors))
        (is (= 1 (length factors)))
        (is (notany (lambda (f) (clause= f parent)) factors))))


(test clover.tests.resolution.factoring.basic
      ;; 基本の因子計算: {P(x), P(a), Q(x)} に mgu {x->A} を適用し、
      ;; 併合後の {P(A), Q(A)} が唯一の因子となること。
      ;; 併合(重複リテラルの除去)は因子の構成に含まれる。
      (is (%factor-list=
            (clover.resolution::factoring
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'P (list (constant 'A)))
                            (literal nil 'Q (list (vterm 'x)))))
              :default)
            (list
              (clause (list (literal nil 'P (list (constant 'A)))
                            (literal nil 'Q (list (constant 'A)))))))))


(test clover.tests.resolution.factoring.does-not-contain-parent
      ;; 因子は親を含まない。また factoring は必ずリテラル数を減らす。
      ;; {P(x), P(y)} の因子は {P(y)} ただ1つ(長さ 1 < 2)。
      (let* ((parent  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (vterm 'y))))))
             (factors (clover.resolution::factoring parent :default)))
        (is (%factor-list=
              factors
              (list (clause (list (literal nil 'P (list (vterm 'y))))))))
        (is (notany (lambda (f) (clause= f parent)) factors))
        (is (every (lambda (f) (< (clause-length f) (clause-length parent))) factors))))


(test clover.tests.resolution.factoring.same-sign-only
      ;; 【同符号条件】factoring は「同符号」リテラル対にのみ適用される。
      ;; 相補的な対 (P(x) と !P(y)) を対象にしてはならない。これを取り違えて
      ;; complement-literal-p で判定すると {P(x), !P(x)} というトートロジーが
      ;; 生成され、simplify で丸ごと捨てられる(= 因子が消える)。
      (is (null (clover.resolution::factoring
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal t   'P (list (vterm 'y)))))
                  :default)))
      ;; 負リテラル同士は同符号なので factoring 対象。{!P(x), !P(y)} -> {!P(y)}
      (is (%factor-list=
            (clover.resolution::factoring
              (clause (list (literal t 'P (list (vterm 'x)))
                            (literal t 'P (list (vterm 'y)))))
              :default)
            (list (clause (list (literal t 'P (list (vterm 'y)))))))))


(test clover.tests.resolution.factoring.multiple-factors-with-dedup
      ;; 【複数因子 + 重複除去 / 実測済み】
      ;; C = P(x,y) v P(a,z) v P(w,b) の同符号対は3組だが、
      ;;   mgu(L1,L2) = {x->A, y->z} -> P(A,z) v P(w,B)
      ;;   mgu(L1,L3) = {x->w, y->B} -> P(A,z) v P(w,B)   <- L1L2 と同一
      ;;   mgu(L2,L3) = {w->A, z->B} -> P(x,y) v P(A,B)
      ;; となり、相異なる因子は2つ。異なる mgu が同一の因子を生むため、
      ;; 重複除去が無いと同型の後継ノードが複数生成される。
      (is (%factor-list=
            (clover.resolution::factoring
              (clause (list (literal nil 'P (list (vterm 'x)   (vterm 'y)))
                            (literal nil 'P (list (constant 'A) (vterm 'z)))
                            (literal nil 'P (list (vterm 'w)   (constant 'B)))))
              :default)
            (list
              (clause (list (literal nil 'P (list (constant 'A) (vterm 'z)))
                            (literal nil 'P (list (vterm 'w)   (constant 'B)))))
              (clause (list (literal nil 'P (list (vterm 'x)   (vterm 'y)))
                            (literal nil 'P (list (constant 'A) (constant 'B)))))))))


(test clover.tests.resolution.factoring.dedup-ignores-variable-names
      ;; 【重複除去は clause= では不十分 / 実測済み】
      ;; C = P(x) v P(y) v P(z) の3対はすべて「P(u) v P(v)」の変種を生むが、
      ;; 束縛の向きが対ごとに違うため、変数名の異なる因子が混在する:
      ;;   mgu(L1,L2) = {x->y} -> P(y) v P(z)
      ;;   mgu(L1,L3) = {x->z} -> P(y) v P(z)
      ;;   mgu(L2,L3) = {y->z} -> P(x) v P(z)
      ;; clause= は literal= の集合差で判定する(equality.lisp:110)ため変数名の違いを
      ;; 吸収できず、2個残る。alphabet-equivalent-p なら1個に畳める。
      (is (%factor-list=
            (clover.resolution::factoring
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'P (list (vterm 'y)))
                            (literal nil 'P (list (vterm 'z)))))
              :default)
            (list
              (clause (list (literal nil 'P (list (vterm 'u)))
                            (literal nil 'P (list (vterm 'v)))))))))


(test clover.tests.resolution.factoring.dedup-ignores-literal-order
      ;; 【重複除去はリテラル順序にも依存してはならない / 実測済み】
      ;; C = P(x) v P(A) v P(y) v P(B) の因子は生では5個で、
      ;;   P(A) v P(y) v P(B)  と  P(x) v P(A) v P(B)
      ;; の2形に分かれる。両者は alphabet-equivalent(相互包摂)だが、
      ;; リテラルの並びが [定数,変数,定数] と [変数,定数,定数] で異なる。
      ;;
      ;; 注: canonical-clause-string はリテラル順序を畳まない(canonicalization.lisp:100)ため、
      ;;   この2形に別キーを与える。remove-duplicates-by-key の前提
      ;;   「eq-fn が真なら key-fn が equal」を破るので、key-fn には使えない。
      ;;   素の (remove-duplicates ... :test #'alphabet-equivalent-p) なら1個に畳める。
      (is (%factor-list=
            (clover.resolution::factoring
              (clause (list (literal nil 'P (list (vterm 'x)))
                            (literal nil 'P (list (constant 'A)))
                            (literal nil 'P (list (vterm 'y)))
                            (literal nil 'P (list (constant 'B)))))
              :default)
            (list
              (clause (list (literal nil 'P (list (constant 'A)))
                            (literal nil 'P (list (vterm 'u)))
                            (literal nil 'P (list (constant 'B)))))))))


(test clover.tests.resolution.factoring.is-one-step-only
      ;; 【1段のみ(閉包を作らない)】
      ;; 上の C の因子 P(A,z) v P(w,B) および P(x,y) v P(A,B) は、いずれも
      ;; さらに factoring すると P(A,B) に収束する。しかし factoring は
      ;; 推論規則として opener_clause-set から毎回適用されるため、反復は探索が担う。
      ;; 1回の呼び出しで閉包を先取りすると、探索と仕事が重複し、深さの異なる因子が
      ;; 同一コストの後継ノードとして並んでしまう。
      ;;
      ;; 注: これは「1段 binary factoring」という設計判断を固定するテストである。
      ;;     閉包を作る設計を採るなら、このテストは削除してよい(他は維持できる)。
      (let ((factors (clover.resolution::factoring
                       (clause (list (literal nil 'P (list (vterm 'x)   (vterm 'y)))
                                     (literal nil 'P (list (constant 'A) (vterm 'z)))
                                     (literal nil 'P (list (vterm 'w)   (constant 'B)))))
                       :default)))
        (is (listp factors))
        (is (notany
              (lambda (f)
                (clause= f (clause (list (literal nil 'P (list (constant 'A)
                                                               (constant 'B)))))))
              factors))))


(test clover.tests.resolution.factoring.snl-mode-yields-nothing
      ;; SNL(Horn/SLD 相当)では factoring は完全性に不要であり、
      ;; :snl では常に空リストを返すこと。これにより opener 側で append しても
      ;; 何も増えず、モード別の条件分岐が不要になる。
      (is (null (clover.resolution::factoring
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal nil 'P (list (vterm 'y)))))
                  :snl)))
      (is (null (clover.resolution::factoring
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal nil 'P (list (constant 'A)))
                                (literal nil 'Q (list (vterm 'x)))))
                  :snl))))


(test clover.tests.resolution.resolution.test1

      (is (let* ((clause1
                   (clause 
                     (list
                       (literal nil 'P (list (vterm 'x) (vterm 'y)))
                       (literal nil 'Q (list (vterm 'x))))))
                 (clause2
                   (clause 
                     (list
                       (literal t 'P (list (vterm 'z) (vterm 'z)))
                       (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                       (literal t 'Q (list (vterm 'w))))))
                 (res 
                   (multiple-value-bind 
                       (a b resoluted)
                       (resolution clause1 clause2 :default)
                     (declare (ignore a b))
                     resoluted))
                 (expected
                   (list 
                     (clause 
                       (list 
                         (literal nil 'Q (list (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                         (literal t 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal t 'Q (list (vterm 'w)))
                         (literal t 'P (list (vterm 'z) (vterm 'z)))
                         (literal nil 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal nil 'P (list (vterm 'w) (vterm 'y))) 
                         (literal t 'P (list (vterm 'z) (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w))))))))))
            (every 
              (lambda (r)
                (member r expected
                  :test #'clause=))
              res)))
  )


(test clover.tests.resolution.resolution.test2
      (let* ((clause1
               (clause (list (literal nil 'P (list (constant 'A )
                                                   (constant 'B )
                                                   (constant 'C )))))
               )
             (clause2
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
                                                   (vterm 'w)))))
               )
             (cs
               (clause-set (list clause1 clause2) :default))
             (ret
               (clover.resolution::resolution-wrapper
                 cs
                 clause1
                 clause2
                 :default
                 (lambda (x) :center)
                 (lambda (x) :resolvent)
                 (lambda (x) :resolvent))))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
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
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (constant 'B )
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'x)
                                                    (constant 'A )
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (constant 'C )
                                                    (vterm 'w)))))))
            ret
            :test #'clause-set=))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
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
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (constant 'C )
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (constant 'B )
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (constant 'A )
                                                    (vterm 'v)
                                                    (vterm 'w)))))))
            ret
            :test #'clause-set=))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
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
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (constant 'C )))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (constant 'B )))
                              (literal t   'P (list (constant 'A )
                                                    (vterm 'y)
                                                    (vterm 'u)))))))
            ret
            :test #'clause-set=))))

