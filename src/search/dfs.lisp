(defpackage clover.search.dfs
  (:use :cl
        :cl-custom-hash-table
        :clover.search.common)
  (:export 
    :dfs
    )
  )
(in-package :clover.search.dfs)


 (define-custom-hash-table-constructor make-node-hash-table
    :test node-equality :hash-function node-hash)


(defmethod dfs ((initial-node abstract-node))
  (let ((foundp nil)
        (result nil)
        (added (make-node-hash-table))
        (stack (list initial-node)))
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
        (with-custom-hash-table
          (loop :for each :in next
                :if (not (gethash each added))
                :do
                (setf (gethash each added) t)
                (push each stack)))))
    (values foundp result)))

