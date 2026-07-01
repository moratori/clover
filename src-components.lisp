;;;; clover のソースファイル構成（共通定義）
;;;;
;;;; clover.asd と clover-build.asd の両方から read-time eval (#.) で読み込まれる。
;;;; これは (:module "src" ... :components <ここ>) に展開される 1 個のリストである。
;;;;
;;;; ソースファイルを追加・削除・並び替えする場合は、このファイルだけを編集すればよい
;;;; （両 .asd を二重に直す必要はない）。
;;;;
;;;; 注意:
;;;;  - このファイルは「データ（リテラルなリスト）」であり、load/compile する用途ではない。
;;;;    各 .asd の :components にも登録しないため、ASDF のコンパイル対象にはならない。
;;;;  - 各 .asd の :around-compile と depends-on は意図的に異なるため、ここには含めない。
;;;;      clover.asd       … sb-cover によるカバレッジ計装 / デバッグ向け最適化
;;;;      clover-build.asd … 警告抑制 / 速度優先の最適化
;;;;  - このファイルを編集しても各 .asd ファイル自身の更新時刻は変わらないため、
;;;;    長時間起動しっぱなしの REPL では再読込が自動で効かないことがある（新規プロセス／
;;;;    CI では .asd が読み直されるので問題ない）。

((:module "lib"
  :components
  ((:file "parallel")
   (:module "search"
    :components
    ((:file "common")
     (:file "iddfs")
     (:file "astar")
     (:file "dfs")))))
 (:file "parameters")
 (:file "conditions")
 (:file "types")
 (:file "equality")
 (:file "canonicalization")
 (:file "logical-predicates")
 (:file "substitute")
 (:file "converter")
 (:file "parser")
 (:file "rename")
 (:file "unify")
 (:file "termorder")
 (:file "simplify")
 (:file "resolution")
 (:file "rewrite")
 (:file "criticalpair")
 (:file "completion")
 (:file "multicompletion")
 (:file "clover")
 (:module "ui"
  :components
  ((:file "rendertree")
   (:file "util")
   (:file "batch")
   (:file "repl")
   (:file "main"))))
