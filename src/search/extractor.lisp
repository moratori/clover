(defpackage clover.search.extractor
  (:use :cl
        :clover.search.common)
  (:import-from :cl-custom-hash-table
                :with-custom-hash-table)
  (:export 
    :extract
    )
  )
(in-package :clover.search.extractor)


(defmethod extract ((initial-node abstract-node) (depth integer))
  (declare (fixnum deepth))
  (assert (>= depth 0))
  (let ((already-opened
          (make-node-hash-table)))
    (labels
        ((inner (node current-depth)
           (with-custom-hash-table
             (cond 
               ((zerop current-depth)
                (list node))
               ((gethash node already-opened) nil)
               (t
                (setf (gethash node already-opened) t)
                (cons node
                      (loop :for each :in (open-nodes node)
                            :append (inner each (1- current-depth)))))))))
      (inner initial-node depth))))

