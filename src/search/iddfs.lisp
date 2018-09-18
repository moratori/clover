(defpackage clover.search.iddfs
  (:use :cl
        :bordeaux-threads
        :cl-cpus)
  (:export 
    :abstract-node
    :open-nodes
    :finish
    :iddfs
    :*minimal-number-of-worker-processors*
    )
  )
(in-package :clover.search.iddfs)


(defparameter *minimal-number-of-worker-processors* 3)


(defun make-thread-pools (maximum-limit number-of-processors)
  (declare (fixnum number-of-processors))
  (let* ((seed 
           (loop for i from 0 upto maximum-limit collect i))
         (len (length seed))
         (number-of-regular-pools
           (floor (/ len number-of-processors)))
         (rest-of-pools
           (mod len number-of-processors)))
    (append
      (loop for i from 1 upto number-of-regular-pools
            for start = (* (1- i) number-of-processors)
            for end   = (+ start number-of-processors)
            collect (subseq seed start end))
      (unless (zerop rest-of-pools)
        (list (subseq seed (- len rest-of-pools) len))))))

(defun run-pool (initial-node pool)
  (loop for deepth in pool
        collect 
        (cons 
          deepth
          (make-thread
            (lambda () (%iddfs-main initial-node deepth))))))

(defun found-pool (pool)
  (let (cnt result) 
    (loop 
      named exit
      for (deepth . thread) in pool
      do 
      (unless (thread-alive-p thread)
        (multiple-value-bind (flag found) (join-thread thread)
          (when flag
            (setf cnt deepth
                  result found)
            (return-from exit nil)))))
    (values cnt result)))

(defun alive-pool-p (pool)
  (some 
    (lambda (thread)
      (destructuring-bind (_ . thread) thread
          (thread-alive-p thread))) 
    pool))


(defstruct abstract-node)

(defmethod open-nodes ((node abstract-node))
  ;; abstract-node のリストを返す関数
  (error "implement for specific method"))

(defmethod finish ((node abstract-node))
  ;; abstract-node を引数にとって t or nil を返す関数
  (error "implement for specific method"))

(defmethod iddfs ((initial-node abstract-node) maximum-limit)
  (declare (fixnum maximum-limit))
  (let* ((number-of-processors 
           (get-number-of-processors))
         (valid-number-of-processors
           (1- number-of-processors)))
    (cond
      ((>= valid-number-of-processors *minimal-number-of-worker-processors*)
       (%iddfs-multi initial-node maximum-limit valid-number-of-processors))
      (t 
       (%iddfs-single initial-node maximum-limit)))))


(defun %iddfs-multi (initial-node maximum-limit number-of-processors)
  (declare (fixnum maximum-limit number-of-processors))
  (let* ((thread-pools
           (make-thread-pools maximum-limit number-of-processors))
         (cnt nil)
         (result nil))
    (loop 
      named exit
      for pool in thread-pools
      for threads-pool = (run-pool initial-node pool)
      do
      (progn
        (loop 
          while (alive-pool-p threads-pool)
          do (progn))
        (multiple-value-bind (local-cnt local-found) (found-pool threads-pool)
          (when local-cnt
            (setf cnt local-cnt
                  result local-found)
            (return-from exit nil)))))
    (values cnt result)))


(defun %iddfs-single (initial-node maximum-limit)
  (declare (fixnum maximum-limit))
  (let (cnt result)
    (loop
      named exit
      for i from 0 upto maximum-limit
      do
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
      (let (flag result)
        (loop 
          named exit
          for each in (open-nodes initial-node)
          do
          (multiple-value-bind 
            (finish-flag node) (%iddfs-main each (1- deepth))
            (when finish-flag 
              (setf 
                flag   t
                result node)
              (return-from exit nil))))
        (values flag result)))))

