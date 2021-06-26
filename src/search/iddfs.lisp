(defpackage clover.search.iddfs
  (:use :cl
        :clover.search.common)
  (:export 
    :iddfs
    )
  )
(in-package :clover.search.iddfs)


(defmethod iddfs ((initial-node abstract-node) maximum-limit)
  (declare (fixnum maximum-limit))
  (let (cnt result)
    (loop
      :named exit
      :for i :from 0 :upto maximum-limit
      do
      (when *debug-print*
        (format *standard-output* "~%##### Iterative Depth = ~A~%" i)
        (force-output *standard-output*))
      (multiple-value-bind
        (flag value) (%iddfs-main initial-node i)
        (when flag
          (setf cnt i
                result value)
          (return-from exit nil))))
    (values cnt result)))

(defun %iddfs-main (initial-node deepth)
  (declare (fixnum deepth))
  (cond 
    ((finish initial-node)
     (values t initial-node))
    ((< deepth 1)
     (values nil nil))
    (t
      (let ((flag nil) 
            (result nil)
            (neighbor (open-nodes initial-node)))

        (when *debug-print*
          (format *standard-output* "parent: ~A~%" initial-node)
          (loop 
            :for node :in neighbor
            :for num :from 1
            :do (format *standard-output* "    child ~A: ~A~%" num node))
          (force-output *standard-output*)
          (sleep 10))

        (loop 
          :named exit
          :for each :in neighbor
          do
          (multiple-value-bind 
            (finish-flag node) (%iddfs-main each (1- deepth))
            (when finish-flag 
              (setf 
                flag   t
                result node)
              (return-from exit nil))))
        (values flag result)))))

