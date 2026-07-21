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
;;;; 導出コアの「完全性ギャップ」に関する受け入れテスト
;;;;
;;;; factoring の導入(節レベル factoring + factoring-wrapper + opener_clause-set
;;;; への結線)により解消されたギャップを固定する回帰テスト。実装は人間側で行われ、
;;;; 本テストはその「あるべき仕様」を固定する。
;;;; ==========================================================================

(test clover.tests.resolution.factoring-incompleteness
      ;; 【factoring による完全性の回復 / 確度: 高 / 実測で GREEN】
      ;; かつて src/ には factoring 規則が無く、:default 導出は「解消リテラルと
      ;; literal=/complement な複製の除去」は行うが、同符号リテラル2本を mgu で
      ;; 1本に併合する本来の factoring を行わなかったため、下の反例で □ に到達できなかった。
      ;; 反例(併せて充足不能):
      ;;   base   : {P(x), P(y)}     (≡ ∀x P(x))
      ;;   conseq : {!P(u), !P(v)}   (≡ ∀u !P(u))
      ;; 二項導出のみでは 2 リテラル節を生み続け □ に到達しない(A* は有限状態を
      ;;   探索し尽くし foundp=NIL。タイムアウトではない)。
      ;; 現在は opener_clause-set が center を factoring するため foundp=T となる。
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
        (is foundp)))


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
;;;; 対象: (clover.resolution::factoring <clause> <mode> <factor-type> <target-type>)
;;;;   戻り値は (values <対象節を target-type で降格したもの> <因子の *リスト*>) の2値。
;;;;   本テスト群は因子の *中身* を検証するため、第2返り値(因子リスト)だけを見る
;;;;   (ヘルパ %factors を使う)。因子リストは 0個・1個・複数個いずれもあり得る。
;;;;   親節そのものは因子に含めない(親の保持は呼び出し側=節集合レベルの責務)。
;;;;   因子には factor-type、降格した親には target-type が適用されるが、
;;;;   ここでは alphabet-equivalent-p / clause= で中身のみ比較するため型は不問。
;;;; ==========================================================================

(defun %factors (clause mode)
  "節レベル factoring の因子リスト(第2返り値)を取り出す。型変換ラムダは
   因子の中身検証に影響しないため、任意のものを渡す。"
  (nth-value 1 (clover.resolution::factoring
                 clause mode
                 (lambda (x) :center)
                 (lambda (x) :resolvent))))

(defun %factor-list= (actual expected)
  "因子リストの一致判定。順序は無視し、変数名の違いは alphabet-equivalent-p で吸収する。"
  (and (listp actual)
       (= (length actual) (length expected))
       (every (lambda (e) (member e actual   :test #'alphabet-equivalent-p)) expected)
       (every (lambda (a) (member a expected :test #'alphabet-equivalent-p)) actual)))


(test clover.tests.resolution.factoring.returns-list
      ;; 因子が存在しない節に対しては、空リスト(NIL)を返すこと。
      ;; 単一リテラル節・同符号だが単一化不能・述語が異なる、のいずれも因子なし。
      ;; 「因子なし」は例外ケースではなく通常経路である(大半の節がこれに該当する)。
      (is (null (%factors
                  (clause (list (literal nil 'P (list (vterm 'x)))))
                  :default)))
      (is (null (%factors
                  (clause (list (literal nil 'P (list (constant 'A)))
                                (literal nil 'P (list (constant 'B)))))
                  :default)))
      (is (null (%factors
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
      (let ((factors (%factors
                       (clause (list (literal nil 'P (list (constant 'A)))
                                     (literal nil 'Q (list (constant 'B)))))
                       :default)))
        (is (null factors)))
      ;; (2) 因子を持つ節でも、自己ペアを含めると親のコピーが余分に混ざる。
      ;;     {P(x), P(a), Q(x)} の真の因子は1つだけである。
      (let* ((parent  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (constant 'A)))
                                    (literal nil 'Q (list (vterm 'x))))))
             (factors (%factors parent :default)))
        (is (listp factors))
        (is (= 1 (length factors)))
        (is (notany (lambda (f) (clause= f parent)) factors))))


(test clover.tests.resolution.factoring.basic
      ;; 基本の因子計算: {P(x), P(a), Q(x)} に mgu {x->A} を適用し、
      ;; 併合後の {P(A), Q(A)} が唯一の因子となること。
      ;; 併合(重複リテラルの除去)は因子の構成に含まれる。
      (is (%factor-list=
            (%factors
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
             (factors (%factors parent :default)))
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
      (is (null (%factors
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal t   'P (list (vterm 'y)))))
                  :default)))
      ;; 負リテラル同士は同符号なので factoring 対象。{!P(x), !P(y)} -> {!P(y)}
      (is (%factor-list=
            (%factors
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
            (%factors
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
            (%factors
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
            (%factors
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
      (let ((factors (%factors
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
      (is (null (%factors
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal nil 'P (list (vterm 'y)))))
                  :snl)))
      (is (null (%factors
                  (clause (list (literal nil 'P (list (vterm 'x)))
                                (literal nil 'P (list (constant 'A)))
                                (literal nil 'Q (list (vterm 'x)))))
                  :snl))))


;;;; ==========================================================================
;;;; factoring-wrapper (節集合レベル) の仕様固定テスト
;;;;
;;;; 対象: (factoring-wrapper <clause-set> <対象節> <mode> <factor-type> <target-type>)
;;;;   戻り値は「後継 *節集合* のリスト」。対象節(=頂節)の因子1つにつき節集合1つ。
;;;;   0個・1個・複数個いずれもあり得る(resolution-wrapper と同じ形状)。
;;;;   各後継節集合 = 対象以外の全節 + 因子(factor-type 適用, 新 :center) +
;;;;                 対象節を target-type で降格したもの(used-cnt +1)。
;;;;
;;;; resolution-wrapper との対称性が要点。本質的な要件は次の3点:
;;;;   - center 限定: 対象(第2引数)だけを factoring する(全節ではない)。
;;;;   - :center はちょうど1つ: 因子を新 :center、対象を :resolvent へ降格する。
;;;;   - 親を捨てない: C |= Cσ だが逆は成り立たないため、降格して残す(置換は不完全)。
;;;;   これにより生成した節集合が opener_clause-set の事前条件を満たす。
;;;; ==========================================================================

(defun %center-count (clause-set)
  "節集合中の :center 節の個数。opener_clause-set :before はこれが 1 を超えると
   multiple-clause-found を送出する(resolution.lisp:313-319)。"
  (count-if (lambda (c) (eq :center (clause.clause-type c)))
            (clause-set.clauses clause-set)))

(defun %factor-succ (clause-set target mode)
  "opener_clause-set が想定する標準呼び出し。因子を新 :center に、対象(親)を
   :resolvent へ降格する(resolution-wrapper が親に行う扱いと対称)。"
  (clover.resolution::factoring-wrapper
    clause-set target mode
    (lambda (x) :center)
    (lambda (x) :resolvent)))


(test clover.tests.resolution.factoring-wrapper.no-factor-yields-nothing
      ;; 対象節が因子を持たなければ、後継ノードを1つも作らないこと(NIL)。
      ;; 探索の大半のノードがこれに該当するため、空リスト(NIL)は通常経路である。
      (let* ((center  (clause (list (literal nil 'P (list (constant 'A))))
                              nil nil nil :center))
             (premise (clause (list (literal t 'Q (list (vterm 'x))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default)))
        (is (null (%factor-succ cs center :default)))))


(test clover.tests.resolution.factoring-wrapper.returns-clause-set-list
      ;; 戻り値は clause-set のリストであること(clause のリストでも、単一の
      ;; clause-set でもない)。opener_clause-set が :append で連結するため、
      ;; resolution-wrapper と同一の形状でなければならない。
      (let* ((center  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (vterm 'y))))
                              nil nil nil :center))
             (premise (clause (list (literal t 'R (list (vterm 'u))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default))
             (ret     (%factor-succ cs center :default)))
        (is (listp ret))
        (is (= 1 (length ret)))
        (is (every (lambda (x) (typep x 'clause-set)) ret))))


(test clover.tests.resolution.factoring-wrapper.successor-preserves-parent-and-adds-factor
      ;; 【親を捨てず降格して残す + 因子を1つ追加 / 確度: 高(健全性の要請)】
      ;; factoring は「追加」の推論規則である。C |= Cσ は成り立つが逆は成り立たない
      ;; (例: C = P(A,z) v P(w,B) は P(A,B) を含意するが、P(A,B) は C を含意しない)。
      ;; したがって親 C を因子で「置き換える」と情報を失い不完全になる。
      ;; resolution-wrapper と対称に、親は :resolvent へ降格して残し、因子を新 :center
      ;; として追加する。結果、節数はちょうど1つ増える。
      ;;
      ;; 注: 因子 {P(v)} と親 {P(x),P(y)} は相互包摂で alphabet-equivalent なため、
      ;;     両者を α同値で区別することはできない。そこで clause-type と
      ;;     リテラル数(clause-length)で判別する。
      (let* ((center  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (vterm 'y))))
                              nil nil nil :center))
             (premise (clause (list (literal t 'R (list (vterm 'u))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default))
             (ret     (%factor-succ cs center :default)))
        (is (= 1 (length ret)))
        (let* ((clauses    (clause-set.clauses (first ret)))
               (centers    (remove-if-not (lambda (c) (eq :center    (clause.clause-type c))) clauses))
               (resolvents (remove-if-not (lambda (c) (eq :resolvent (clause.clause-type c))) clauses))
               (premises   (remove-if-not (lambda (c) (eq :premise   (clause.clause-type c))) clauses)))
          ;; 因子は新 :center。ちょうど1つで、親より短い(因子は必ずリテラルを減らす)
          (is (= 1 (length centers)))
          (is (< (clause-length (first centers)) (clause-length center)))
          ;; 親は捨てられず :resolvent へ降格して残る(リテラル数は親と同じ)
          (is (= 1 (length resolvents)))
          (is (= (clause-length center) (clause-length (first resolvents))))
          ;; 前提はそのまま残る
          (is (= 1 (length premises)))
          ;; 増えるのはちょうど1節(因子を複数まとめて1ノードに詰め込まない)
          (is (= (1+ (length (clause-set.clauses cs)))
                 (length clauses))))))


(test clover.tests.resolution.factoring-wrapper.center-count-is-exactly-one
      ;; 【:center はちょうど1つ / 確度: 事実(システム不変条件)】
      ;; 標準呼び出し(因子->:center, 親->:resolvent)では、各後継節集合の :center は
      ;; ちょうど1つでなければならない。
      ;;   - 2 以上: opener_clause-set :before (resolution.lisp:313-319) が
      ;;             multiple-clause-found を送出する。
      ;;   - 0     : :default opener は center-clause が NIL のとき何も返さない
      ;;             (resolution.lisp:329 の when)ので、そのノードは死に枝になる。
      (let* ((center  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (vterm 'y))))
                              nil nil nil :center))
             (premise (clause (list (literal t 'R (list (vterm 'u))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default))
             (ret     (%factor-succ cs center :default)))
        (is (= 1 (length ret)))
        (is (every (lambda (succ) (= 1 (%center-count succ))) ret))))


(test clover.tests.resolution.factoring-wrapper.successor-is-openable
      ;; 【生成した節集合はそのまま展開できること / 確度: 事実(実測で再現)】
      ;; factoring-wrapper の戻り値は open リストに積まれ、次の展開で
      ;; opener_clause-set に渡される。よって opener の事前条件を満たす必要がある。
      ;;
      ;; 3リテラルの頂節(相異なる因子を2つ持つ)+ 前提 !P(A,B) を用いる。
      ;;   C  = P(x,y) v P(A,z) v P(w,B)              (:center)
      ;; 因子は mgu(L1,L2)/mgu(L1,L3) 由来の P(A,z) v P(w,B) と、
      ;; mgu(L2,L3) 由来の P(x,y) v P(A,B) の2つ(前者2つは α同値で1つに畳まれる)。
      ;; 前提 !P(A,B) は、純リテラル消去で節集合が空になるのを防ぐために置いている。
      ;;
      ;; 注: clover の各コンディションは clover-toplevel-condition 直下で
      ;;   cl:error のサブタイプではない(conditions.lisp:20)。よって
      ;;   handler-case の節は error ではなく condition で受ける必要がある。
      (let* ((center  (clause (list (literal nil 'P (list (vterm 'x)     (vterm 'y)))
                                    (literal nil 'P (list (constant 'A) (vterm 'z)))
                                    (literal nil 'P (list (vterm 'w)    (constant 'B))))
                              nil nil nil :center))
             (premise (clause (list (literal t 'P (list (constant 'A) (constant 'B))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default))
             (ret     (%factor-succ cs center :default)))
        (is (= 2 (length ret)))
        (is (every (lambda (succ) (= 1 (%center-count succ))) ret))
        (is (every (lambda (succ)
                     (handler-case (progn (opener_clause-set succ :default) t)
                       (condition (c) (declare (ignore c)) nil)))
                   ret))))


(test clover.tests.resolution.factoring-wrapper.factors-only-target
      ;; 【center 限定 / 確度: 高(設計判断)】
      ;; factoring-wrapper は「対象(第2引数=頂節)」だけを factoring する。対象以外の
      ;; 節(前提など)は、たとえ因子を持っていても factoring しない。
      ;; center {P(x),P(y)} と premise {Q(u),Q(v)} はいずれも因子を1つ持つが、
      ;; 対象= center とした呼び出しでは後継はちょうど1ノード(center の因子1つ分)。
      ;; ※もし「全節を factoring」する実装なら、ここは 2 になる(前提由来の後継が増える)。
      (let* ((center  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (vterm 'y))))
                              nil nil nil :center))
             (premise (clause (list (literal nil 'Q (list (vterm 'u)))
                                    (literal nil 'Q (list (vterm 'v))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default))
             (ret     (%factor-succ cs center :default)))
        ;; center 由来の因子1つ分、後継はちょうど1ノード
        (is (= 1 (length ret)))
        ;; 前提は factoring されず、2リテラルのまま残っている
        (let ((prem (find-if (lambda (c) (eq :premise (clause.clause-type c)))
                             (clause-set.clauses (first ret)))))
          (is (not (null prem)))
          (is (= 2 (clause-length prem))))))


(test clover.tests.resolution.factoring-wrapper.respects-type-transformers
      ;; 【型・used-cnt は呼び出し側/機構が決める / 確度: 事実(実測)】
      ;; resolution と対称に、因子の型は factor-type、降格した親の型は target-type で
      ;; 決まり(節側にハードコードしない)、親の used-cnt は 1 増える。
      ;; 標準と異なる型(:conseq)を渡して、確かに反映されることを固定する。
      (let* ((center  (clause (list (literal nil 'P (list (vterm 'x)))
                                    (literal nil 'P (list (vterm 'y))))
                              nil nil nil :center 3))   ; used-cnt = 3
             (premise (clause (list (literal t 'R (list (vterm 'u))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :default))
             (ret     (clover.resolution::factoring-wrapper
                        cs center :default
                        (lambda (x) :conseq)        ; 因子   -> :conseq
                        (lambda (x) :resolvent))))   ; 親(降格) -> :resolvent
        (is (= 1 (length ret)))
        (let* ((clauses (clause-set.clauses (first ret)))
               ;; 因子: リテラル1 かつ述語 P(前提 R とは述語で区別)
               (factor (find-if (lambda (c)
                                  (and (= 1 (clause-length c))
                                       (eq 'P (literal.predicate (first (clause.literals c))))))
                                clauses))
               ;; 降格した親: リテラル2 の節はこれだけ
               (org    (find-if (lambda (c) (= 2 (clause-length c))) clauses)))
          ;; 因子には factor-type が適用される
          (is (not (null factor)))
          (is (eq :conseq (clause.clause-type factor)))
          ;; 降格した親には target-type が適用され、used-cnt が 1 増える
          (is (not (null org)))
          (is (eq :resolvent (clause.clause-type org)))
          (is (= 4 (clause.used-cnt org))))))


(test clover.tests.resolution.factoring-wrapper.snl-yields-nothing
      ;; :snl では節レベルの factoring が(第2返り値=)因子を常に NIL とするため、
      ;; 節集合レベルでも後継ノードは生成されない。opener 側で無条件に append できる。
      (let* ((center  (clause (list (literal t 'P (list (vterm 'x)))
                                    (literal t 'P (list (vterm 'y))))
                              nil nil nil :center))
             (premise (clause (list (literal nil 'P (list (vterm 'u))))
                              nil nil nil :premise))
             (cs      (clause-set (list center premise) :snl)))
        (is (null (%factor-succ cs center :snl)))))


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
                       (resolution clause1 clause2 :default
                                   (lambda (x) :resolvent)
                                   (lambda (x) :resolvent)
                                   (lambda (x) :resolvent)
                                   )
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

