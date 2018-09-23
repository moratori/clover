(defpackage clover.conditions
  (:use :cl)
  (:export
    :clover-toplevel-condition
    :unexpected-error
    :occurrence-check-error
    :ununifiable-literal-error
    :unmatching-fterm-error
    :unmatching-literal-error
    :unexpected-unifier-source
    :message-of
    :unimplemented-resolution-algorithm
    )
  )
(in-package :clover.conditions)



(define-condition clover-toplevel-condition ()
  ((message
     :initform ""
     :initarg :message
     :accessor message-of)))


(define-condition unexpected-error (clover-toplevel-condition)
  ((values
     :initarg :values)))


(define-condition occurrence-check-error (clover-toplevel-condition)
  ((vterm 
     :initarg :vterm
     :accessor vterm-of)
   (fterm
     :initarg :fterm
     :accessor fterm-of)))


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

(define-condition unexpected-unifier-source (clover-toplevel-condition)
  ((src
     :initarg :src
     :initform nil
     :accessor src-of)))

(define-condition unimplemented-resolution-algorithm (clover-toplevel-condition)
  ())

(define-condition null-clause-not-found (clover-toplevel-condition)
  ())


(defmethod print-object ((error clover-toplevel-condition) stream)
  (format stream "~%~A~%" (message-of error)))

