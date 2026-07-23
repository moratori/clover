(defpackage clover.lib.util
  (:use :cl)
  (:import-from :generators
                :make-generator
                :yield
                :next
                :stop-iteration)
  (:export
    :pairwise-collect-if
    :permutation
    :take  
    ))
(in-package :clover.lib.util)


(defun pairwise-collect-if (fn lst)
  (loop :for (a . rest) :on lst
        :nconc (let ((acc nil))
                 (dolist (b rest (nreverse acc))
                   (multiple-value-bind (flag value) (funcall fn a b)
                     (when flag (push value acc)))))))
 

(defun permutation (elements)
  (make-generator ()
    (if (<= (length elements) 1)
        (yield elements)
        (handler-case
            (loop
              :with gen := (permutation (subseq elements 1))
              :for perm := (next gen)
              :do
              (loop
                :for i :from 0 :below (length elements)
                :do
                (yield 
                  (append 
                    (subseq perm 0 i)
                    (subseq elements 0 1)
                    (subseq perm i)))))
          (stop-iteration (c) 
            (declare (ignore c)) nil)))))

(defun take (n gen)
  (let (result)
    (handler-case 
        (dotimes (i n)
          (let ((value (next gen)))
            (when value
              (push value result))))
      (stop-iteration (c)
        (declare (ignore c))))
    result))
