(defpackage clover.lib.search.dfs
  (:use :cl
        :clover.lib.search.common)
  (:export
    :dfs
    )
  )
(in-package :clover.lib.search.dfs)



(defmethod dfs ((initial-node abstract-node))
  (let ((foundp nil)
        (result nil)
        ;; closed 集合: 各ノードの正準キー(node-canonical-key)を equal ハッシュで保持し、
        ;; 既出と同一視できる状態の再 push を抑制する。閉路を含むグラフでの停止性を担保する。
        ;; キーの同値性は node-canonical-key の実装に委ねる(astar と同方式)。
        (seen (make-hash-table :test #'equal))
        (stack (list initial-node)))
    (setf (gethash (node-canonical-key initial-node) seen) t)
    (loop
      :named exit
      :while (not (null stack))
      :do
      (let* ((target (pop stack))
             (next (open-nodes target)))
        (loop
          :for each :in (cons target next)
          :if (finish each)
          :do
          (setf foundp t
                result each)
          (return-from exit))
        (loop :for each :in next
              :for key := (node-canonical-key each)
              :unless (gethash key seen)
              :do
              (setf (gethash key seen) t)
              (push each stack))))
    (values foundp result)))
