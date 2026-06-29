(defpackage clover.search.astar
  (:use :cl
        :clover.search.common)
  (:import-from :cl-heap
                :enqueue
                :dequeue
                :queue-size
                :priority-queue)
  (:export
    :cost-to-goal
    :cost-to-neighbor
    :astar))
(in-package :clover.search.astar)


(defconstant +POSITIVE-INFINITY+
  #+allegro   excl:*infinity-double*
  #+sbcl      sb-ext:double-float-positive-infinity
  #+(or lispworks clozure) +1D++0
  )


(defmethod cost-to-goal ((node abstract-node))
  (error "implement for specific method"))

(defmethod cost-to-neighbor ((node1 abstract-node) (node2 abstract-node))
  (error "implement for specific method"))


(defmethod astar ((initial-node abstract-node))
  (let ((gScore (make-hash-table))
        ;; closed 集合: 各ノードの正準キー(node-canonical-key)を equal ハッシュで保持し、
        ;; 既出と同一視できる状態の再エンキューを抑制する。キーの同値性は node-canonical-key の
        ;; 実装に委ねる(デフォルトはノード自身=実質重複排除なし)。
        (seen (make-hash-table :test #'equal))
        (queue (make-instance 'priority-queue))
        foundp result)

    (setf (gethash initial-node gScore) 0)
    (setf (gethash (node-canonical-key initial-node) seen) t)
    (enqueue queue initial-node (cost-to-goal initial-node))

    (loop
      :named exit
      :while (> (queue-size queue) 0)
      :for current := (dequeue queue)
      :if (finish current)
      :do
      (progn
        (setf foundp t
              result current)
        (return-from exit))
      :else
      :do
      (loop
        :for neighbor :in (open-nodes current)
        :for key := (node-canonical-key neighbor)
        :unless (gethash key seen)
        :do
        (let ((tentative-gscore
                (+ (cost-to-neighbor current neighbor)
                   (gethash current gScore +POSITIVE-INFINITY+))))
          (setf (gethash key seen) t
                (gethash neighbor gScore) tentative-gscore)
          (enqueue queue neighbor (+ tentative-gscore (cost-to-goal neighbor))))))

    (values foundp result)))

