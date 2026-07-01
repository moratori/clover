;;;; 完備化コーパステスト
;;;;
;;;; 出典 (source):
;;;;   JAIST Maximal Completion 実験ページの等式系ベンチマーク
;;;;   https://www.jaist.ac.jp/project/maxcomp/experiments/experiments.html
;;;;   各 .trs は ./eq_systems/ 配下のファイルで、tests/resources/eq_systems/ に同梱している。
;;;;
;;;; 本ファイルのテストは、上記コーパスのうち「現行 clover が
;;;; *completion-giveup-threshold* (=15) の下で、独立プロセス計測で 1 秒未満かつ
;;;; 安定して完備化に成功した 50 問」に限定している。
;;;; 完備化は記号順序の並列探索を含み実行時間に変動があるため、時間がかかる問題
;;;; (1 秒以上) は flaky を避ける目的で意図的に除外している。
;;;;
;;;; アサーションは「完備化が成功すること（toplevel-completion の成功フラグが真）」のみ。
;;;; 完備化後の正規系は順序探索の非決定性により一意でないため、内容は固定しない。

(defpackage clover.tests.completion.corpus
  (:use :cl :1am)
  (:import-from :clover.parser
                :parse-mkbtt-expression)
  (:import-from :clover.multicompletion
                :toplevel-completion)
  (:import-from :clover.parameters
                :*completion-giveup-threshold*))
(in-package :clover.tests.completion.corpus)


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


(defun %corpus-completes-p (filename)
  ;; 同梱 .trs をパースして equation-set にし、CLI と同じ閾値で完備化を試みる。
  ;; 成功フラグ（toplevel-completion の第1返り値）を返す。
  (let ((equation-set (parse-mkbtt-expression (%read-corpus-file filename))))
    (values (toplevel-completion equation-set *completion-giveup-threshold*))))


(defmacro define-corpus-completion-tests (&rest filenames)
  ;; 各ファイル名につき「完備化が成功する」ことを検証するテストを1つ生成する。
  `(progn
     ,@(mapcar
         (lambda (fn)
           `(test ,(intern (concatenate 'string
                             "CLOVER.TESTS.COMPLETION.CORPUS." (%test-name-of fn)))
              (is (%corpus-completes-p ,fn))))
         filenames)))


(define-corpus-completion-tests
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
