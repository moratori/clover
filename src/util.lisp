(defpackage clover.util
  (:use :cl
        :clover.conditions
        :clover.types)
  )
(in-package :clover.util)



(defmethod term= ((term1 vterm) (term2 vterm))
  (eq (vterm.var term1) (vterm.var term2)))


(defmethod term= ((fterm1 fterm) (fterm2 fterm))
  )
