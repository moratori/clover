;;;; 完備化コーパス: 生成規則の内容テスト
;;;;
;;;; completion-corpus.lisp が「完備化の成功フラグ」のみを検証するのに対し、
;;;; 本ファイルは tests/tools/generate-completion-expected.lisp が生成した
;;;; 期待値ファイル (tests/resources/eq_systems_expected/<base>.lisp) を用いて、
;;;; 記号順序を固定した kb-completion を直接実行し、次の3点を検証する。
;;;;
;;;;   (1) 完備化が成功すること
;;;;   (2) 生成された書き換え規則集合が期待値とα同値であること
;;;;       （規則ごとの変数リネームとリスト内の並び順を除いて一致）
;;;;   (3) 入力の各等式の左右辺が、生成された規則集合の rewrite-final で
;;;;       同じ正規形に合流すること（完備化の健全性の性質検査）
;;;;
;;;; 決定性について: 完備化の非決定性は multi-kb-completion の psome/kill による
;;;; 記号順序の並列レースのみが源であり、順序を固定した kb-completion は逐次かつ
;;;; 決定的（completion/rewrite/criticalpair/termorder に並列・乱数・ハッシュ順
;;;; 依存なし）。よって期待値との厳密な比較が可能。
;;;;
;;;; 記号の復元について: LPO は ordering 内の記号を eq で検索する
;;;; （termorder.lisp の position :test #'eq）ため、期待値ファイルの記号名
;;;; （文字列）を任意のパッケージに intern し直しても機能しない。パース済み
;;;; equation-set から収集した実シンボルへ symbol-name 照合で解決する。
;;;;
;;;; 期待値の再生成（ベースライン張り替え）は tests/tools/ のスクリプトで行う。

(defpackage clover.tests.completion.corpus-rules
  (:use :cl
        :clover.types
        :clover.completion
        :1am)
  (:import-from :clover.parser
                :parse-mkbtt-expression)
  (:import-from :clover.parameters
                :*term-order-algorithm*)
  (:import-from :clover.equality
                :term=)
  (:import-from :clover.unify
                :alphabet-equivalent-p)
  (:import-from :clover.canonicalization
                :canonical-rewrite-rule-key)
  (:import-from :clover.rewrite
                :rewrite-final))
(in-package :clover.tests.completion.corpus-rules)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %test-name-of (filename)
    ;; "SK90_3.02.trs" -> "SK90_3_02" のように、拡張子を除き
    ;; 英数字と _ 以外（. や -）を _ に置換して大文字化したテスト名を作る。
    (let ((base (if (and (> (length filename) 4)
                         (string-equal ".trs" (subseq filename (- (length filename) 4))))
                    (subseq filename 0 (- (length filename) 4))
                    filename)))
      (string-upcase
        (map 'string
             (lambda (c) (if (or (alphanumericp c) (char= c #\_)) c #\_))
             base)))))


(defun %read-corpus-file (filename)
  (let ((path (asdf:system-relative-pathname
                :clover-test
                (concatenate 'string "tests/resources/eq_systems/" filename))))
    (with-open-file (s path :direction :input)
      (let* ((buf (make-string (file-length s)))
             (n (read-sequence buf s)))
        (subseq buf 0 n)))))


(defun %read-expected-file (filename)
  ;; 期待値ファイルは keyword・文字列・数値のみで構成されるため、
  ;; 読み込み時のパッケージに依存しない。
  (let* ((base (subseq filename 0 (- (length filename) 4)))
         (path (asdf:system-relative-pathname
                 :clover-test
                 (concatenate 'string "tests/resources/eq_systems_expected/"
                              base ".lisp"))))
    (with-open-file (s path :direction :input)
      (let ((*read-eval* nil))
        (read s)))))


(defun %find-named-symbol (name symbols filename)
  ;; find だと「シンボル NIL が見つかった」と「見つからない」を区別できない
  ;; （SK90_3.21.trs 等は定数 nil を含む）ため、member の返すテイルで判定する。
  (let ((tail (member name symbols :key #'symbol-name :test #'string=)))
    (if tail
        (car tail)
        (error "期待値ファイルの記号 ~S に対応するシンボルが ~A のパース結果にありません"
               name filename))))


(defun %build-term (sexp constants functions filename)
  ;; 期待値ファイルの項表現から実際の term を再構成する。
  ;;   (:var N) -> vterm（番号ごとに同名のシンボルへ intern。α同値比較なので
  ;;               変数シンボルの同一性は結果に影響しない）
  ;;   "NAME"   -> constant（パース済みの実シンボルへ名前照合で解決）
  ;;   ("NAME" arg ...) -> fterm（同上）
  (cond
    ((stringp sexp)
     (constant (%find-named-symbol sexp constants filename)))
    ((and (consp sexp) (eq (first sexp) :var))
     (vterm (intern (format nil "?~A" (second sexp))
                    :clover.tests.completion.corpus-rules)))
    ((and (consp sexp) (stringp (first sexp)))
     (fterm (%find-named-symbol (first sexp) functions filename)
            (mapcar
              (lambda (arg) (%build-term arg constants functions filename))
              (rest sexp))))
    (t
     (error "期待値ファイル(~A)の項表現が不正です: ~S" filename sexp))))


(defun %run-corpus-rules-check (filename)
  (let* ((expected (%read-expected-file filename))
         (equation-set (parse-mkbtt-expression (%read-corpus-file filename)))
         (*term-order-algorithm* (getf expected :term-order-algorithm)))
    (multiple-value-bind (constants functions)
        (clover.multicompletion::collect-symbol equation-set)
      (let* ((all-symbols (append constants functions))
             (ordering (function-symbol-ordering
                         (mapcar
                           (lambda (name)
                             (%find-named-symbol name all-symbols filename))
                           (getf expected :ordering))))
             (expected-rules
               (mapcar
                 (lambda (rule-sexp)
                   (rewrite-rule
                     (%build-term (first rule-sexp) constants functions filename)
                     (%build-term (second rule-sexp) constants functions filename)))
                 (getf expected :rules))))
        (multiple-value-bind (flag result-ordering rrs)
            (kb-completion equation-set ordering (getf expected :giveup-threshold))
          (declare (ignore result-ordering))
          ;; (1) 記録済みの記号順序の下で完備化が成功する
          (is flag)
          ;; (2) 生成された規則集合が期待値とα同値
          (let* ((actual-rules (rewrite-rule-set.rewrite-rules rrs))
                 (missing (set-difference expected-rules actual-rules
                                          :test #'alphabet-equivalent-p))
                 (extra (set-difference actual-rules expected-rules
                                        :test #'alphabet-equivalent-p)))
            (when (or missing extra)
              ;; アサーション失敗時に過不足を canonical キーで表示する
              ;; （生成規則の変数は gensym 名のため、リネーム不変なキー表記で
              ;;   期待値ファイルの行と突き合わせられるようにする）。
              (format t "~&[~A] 生成規則が期待値と不一致~%" filename)
              (format t "  期待にあるが生成されず:~%~{    ~A~%~}"
                      (mapcar #'canonical-rewrite-rule-key missing))
              (format t "  生成されたが期待にない:~%~{    ~A~%~}"
                      (mapcar #'canonical-rewrite-rule-key extra)))
            (is (alphabet-equivalent-p (rewrite-rule-set expected-rules) rrs)))
          ;; (3) 入力の各等式の左右辺が同じ正規形に合流する
          (loop :for equation :in (equation-set.equations equation-set)
                :unless (equation.negation equation)
                :do (let ((nf-left (rewrite-final (equation.left equation) rrs))
                          (nf-right (rewrite-final (equation.right equation) rrs)))
                      (unless (term= nf-left nf-right)
                        (format t "~&[~A] 等式 ~A の左右辺が合流しません: ~A vs ~A~%"
                                filename equation nf-left nf-right))
                      (is (term= nf-left nf-right)))))))))


(defmacro define-corpus-rules-tests (&rest filenames)
  ;; 各ファイル名につき、期待値ファイルとの照合テストを1つ生成する。
  `(progn
     ,@(mapcar
         (lambda (fn)
           `(test ,(intern (concatenate 'string
                             "CLOVER.TESTS.COMPLETION.CORPUS-RULES." (%test-name-of fn)))
              (%run-corpus-rules-check ,fn)))
         filenames)))


(define-corpus-rules-tests
    "ASK93_1.trs"
    "ASK93_6.trs"
    "aufgabe3_2.trs"
    "aufgabe3_3.trs"
    "BD94_collapse.trs"
    "BD94_peano.trs"
    "BD94_sqrt.trs"
    "BH96_fac8_theory.trs"
    "fggx.trs"
    "KK99_linear_assoc.trs"
    "Les83_fib.trs"
    "Les83_subset.trs"
    "OKW95_dt1_theory.trs"
    "Sim91_sims2.trs"
    "SK90_3.02.trs"
    "SK90_3.08.trs"
    "SK90_3.10.trs"
    "SK90_3.11.trs"
    "SK90_3.13.trs"
    "SK90_3.14.trs"
    "SK90_3.16.trs"
    "SK90_3.17.trs"
    "SK90_3.18.trs"
    "SK90_3.20.trs"
    "SK90_3.21.trs"
    "SK90_3.23.trs"
    "SK90_3.24.trs"
    "SK90_3.25.trs"
    "SK90_3.28.trs"
    "SK90_3.29.trs"
    "SK90_3.30.trs"
    "SK90_3.31.trs"
    "SK90_3.32.trs"
    "SK90_3.33.trs"
    "slothrop_ackermann.trs"
    "slothrop_fgh.trs"
    "slothrop_groups_conj.trs"
    "slothrop_hard.trs"
    "TPDB_secret2006_torpa_secr10.trs"
    "TPDB_secret2006_torpa_secr4.trs"
    "TPDB_zantema_z115.trs"
    "TPTP_BOO027-1_theory.trs"
    "TPTP_COL053-1_theory.trs"
    "TPTP_COL056-1_theory.trs"
    "TPTP_COL085-1_theory.trs"
    "TPTP_GRP012-4_theory.trs"
    "TPTP_GRP393-2_theory.trs"
    "TPTP_HWC004-1_theory.trs"
    "TPTP_HWC004-2_theory.trs"
    "TPTP_SWV262-2_theory.trs")
