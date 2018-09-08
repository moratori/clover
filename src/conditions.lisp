(defpackage clover.conditions
  (:use :cl)
  (:export
    :occurrence-check-error
    )
  )
(in-package :clover.conditions)



(define-condition clover-toplevel-condition ()
  ((message
     :initarg :message)))



(define-condition occurrence-check-error (clover-toplevel-condition)
  ((vterm 
     :initarg :vterm)
   (fterm
     :initarg :fterm)))


(define-condition unmatching-fterm (clover-toplevel-condition)
  ((fterm1
     :initarg :fterm1)
   (fterm2
     :initarg :fterm2)))

(define-condition unmatching-literal (clover-toplevel-condition)
  ((literal1
     :initarg :literal1)
   (literal2
     :initarg :literal2)))
