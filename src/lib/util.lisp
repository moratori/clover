(defpackage clover.lib.util
  (:use :cl)
  (:export
    :pairwise-collect-if
    ))
(in-package :clover.lib.util)


(defun pairwise-collect-if (fn lst)
  (loop :for (a . rest) :on lst
        :nconc (let ((acc nil))
                 (dolist (b rest (nreverse acc))
                   (multiple-value-bind (flag value) (funcall fn a b)
                     (when flag (push value acc)))))))
 
