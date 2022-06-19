(defpackage clover.multiprocess
  (:use :cl)
  (:export
    :initialize-lparallel-kernel
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
  (when lparallel:*kernel*
    (lparallel:end-kernel :wait nil))
  (unless lparallel:*kernel*
    (let ((cpu-number
            (get-number-of-processors)))
      (setf lparallel:*kernel* 
            (lparallel:make-kernel
              (if (>= 1 cpu-number) 1 (1- cpu-number))))))) 
