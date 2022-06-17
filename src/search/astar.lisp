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
        (queue (make-instance 'priority-queue))
        foundp result)

    (setf (gethash initial-node gScore) 0)
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
        :for tentative-gscore := (+ (cost-to-neighbor current neighbor) 
                                    (gethash current gScore +POSITIVE-INFINITY+))
        :if (< tentative-gscore 
               (gethash neighbor gScore +POSITIVE-INFINITY+))
        :do
        (progn
          (setf (gethash neighbor gScore) tentative-gscore)
          (enqueue queue neighbor (+ tentative-gscore (cost-to-goal neighbor))))))

    (values foundp result)))

