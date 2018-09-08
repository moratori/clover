(defpackage clover.conditions
  (:use :cl)
  (:export
    :clover-toplevel-condition
    :unexpected-error
    :occurrence-check-error
    :unmatching-fterm-error
    :unmatching-literal-error
    )
  )
(in-package :clover.conditions)



(define-condition clover-toplevel-condition ()
  ((message
     :initform ""
     :initarg :message)))


(define-condition unexpected-error (clover-toplevel-condition)
  ((values
     :initarg :values)))


(define-condition occurrence-check-error (clover-toplevel-condition)
  ((vterm 
     :initarg :vterm)
   (fterm
     :initarg :fterm)))


(define-condition unmatching-fterm-error (clover-toplevel-condition)
  ((fterm1
     :initarg :fterm1)
   (fterm2
     :initarg :fterm2)))

(define-condition unmatching-literal-error (clover-toplevel-condition)
  ((literal1
     :initarg :literal1)
   (literal2
     :initarg :literal2)))

(define-condition ununifiable-literal-error (clover-toplevel-condition)
  ((literal1
     :initarg :literal1)
   (literal2
     :initarg :literal2)))
