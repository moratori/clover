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


(defmethod extract ((initial-node abstract-node) (depth integer) unique)
  (declare (fixnum deepth))
  (assert (>= depth 0))
  (let ((already-opened
          (make-equal-node-hash-table)))
    (labels
        ((inner (node limit)
           (with-custom-hash-table
             (cond 
               ((zerop limit)
                (list node))
               ((gethash node already-opened) nil)
               (t
                (setf (gethash node already-opened) t)
                (let ((neighbor
                        (open-nodes node)))
                  (when *debug-print*
                    (format *standard-output* "~%##### Depth Limit = ~A~%" limit)
                    (format *standard-output* "Found Nodes = ~A~%" (length neighbor))
                    (force-output *standard-output*))
                  (cons node
                        (loop :for each :in neighbor
                              :for raw := (inner each (1- limit))
                              :append raw))))))))
      (let ((result (inner initial-node depth)))
        (if unique
            (remove-duplicates result :test #'node-same-class)
            result)))))

