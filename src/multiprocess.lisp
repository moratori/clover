(defpackage clover.multiprocess
  (:use :cl)
  (:export
    :initialize-lparallel-kernel
    :psome/kill
    ))
(in-package :clover.multiprocess)


(let (cached)
  (defun get-number-of-processors ()
    (if cached cached
        (let ((num
                (handler-case
                    (cpus:get-number-of-processors)
                  (error (c) 1))))
          (setf cached num)))))


(defparameter +lost-worker-warning-substring+ "Replacing lost or dead worker"
  "lparallel が破棄されたワーカーを補充する際に出す警告メッセージ
   (src/kernel/core.lisp の `(warn \"lparallel: Replacing lost or dead worker.\")`) の判定用部分文字列。")

(defun %muffle-lost-worker-warning (condition)
  "上記の『ワーカー補充』警告だけを抑制する。`psome/kill' は kill-tasks で非勝者ワーカーを
   故意に破棄するため、その補充に伴う本警告は想定内のノイズである。メッセージが一致しない
   警告には何もせず通過させる(=他の警告や『Worker replacement failed』等は従来どおり出る)。"
  (when (search +lost-worker-warning-substring+ (princ-to-string condition))
    (muffle-warning condition)))

(defun %worker-context (worker-loop)
  "make-kernel の :context フック。各ワーカースレッドのループ実行を handler-bind で包む。
   問題の warn は『ワーカースレッド側』で signal されるため、メインスレッドの handler-bind
   では捕捉できない。本フックはワーカーループの動的環境内(その :abort 後始末も含む)で
   ハンドラを張るため、当該スレッドで発生する警告を捕捉・抑制できる。
   worker-loop はワーカーが終了するまで戻らない関数で、必ず funcall する必要がある。"
  (handler-bind ((warning #'%muffle-lost-worker-warning))
    (funcall worker-loop)))

(defun initialize-lparallel-kernel ()
  ;; カーネルは「生成が高価, プロセス存続中に1個」という lparallel 推奨の使い方に従い、
  ;; 既に在ればそれを再利用する(冪等)。以前は毎回 end-kernel :wait nil → make-kernel で
  ;; 作り直していたため、(1) 呼出毎のスレッドプール再生成コスト、(2) :wait nil による
  ;; 旧worker破棄と新worker生成の重なり(スレッド急増)、が生じていた。再利用でこれらを解消する。
  (unless lparallel:*kernel*
    (let ((cpu-number
            (get-number-of-processors)))
      (setf lparallel:*kernel*
            (lparallel:make-kernel
              (if (>= 1 cpu-number) 1 (1- cpu-number))
              ;; kill-tasks による非勝者ワーカー破棄→補充時の警告を抑制する(後述フック)。
              :context #'%worker-context)))))


(defun psome/kill (predicate &rest sequences)
  "`lparallel.cognate:psome' の派生版。最初に非 nil を返したタスク(勝者)が出ると psome は
   短絡して返るが、既に走り出している非勝者タスクはそのまま走り続ける
   (psome 内部の打ち切りは『まだ開始していない』タスクしか止められないため)。
   本関数は psome に渡す各タスクへ専用のタスクカテゴリを付与し、psome から戻った後に
   そのカテゴリで実行中のタスクを `kill-tasks' で強制終了して非勝者を直ちに止める。
   これにより『勝者確定後も走り続ける敗者が後続処理と CPU/メモリ帯域を奪い合う』のを防ぐ。
   completion に限らず『複数を並列に試し最初の成功で残りを止めたい』一般用途に使える。

   重要な前提と注意:
   - `kill-tasks' はワーカースレッドを `destroy-thread' で非同期に中断・再生成する強制
     キャンセルである。したがって predicate が表す処理は『副作用を持たず, 任意の地点で
     中断されても共有状態を壊さない(ロック非保持・大域変更なし)』ことが前提。
   - lparallel の `kill-tasks' は『例外的状況向け』とされるが, ここでは専用カテゴリによる
     狙い撃ち kill であり, 公式が想定する使い方の範囲。ABCL では kill-tasks 非対応。"
  (let ((category (cons :psome/kill nil)))   ; eql で一意に識別できるフレッシュなカテゴリ
    (unwind-protect
        (let ((lparallel:*task-category* category))
          (apply #'lparallel.cognate:psome predicate sequences))
      (lparallel:kill-tasks category)))) 
