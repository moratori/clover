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
              (if (>= 1 cpu-number) 1 (1- cpu-number)))))))


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
