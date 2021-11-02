(defpackage clover.conditions
  (:use :cl)
  (:export
    :clover-toplevel-condition
    :unexpected-error
    :occurrence-check-error
    :ununifiable-error
    :unmatching-fterm-error
    :unmatching-literal-error
    :unexpected-unifier-source
    :message-of
    :null-clause-not-found
    :multiple-clause-found
    :expr-parse-error
    :mkbtt-parse-error
    :unable-to-orient-equation
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

(define-condition ununifiable-error (clover-toplevel-condition)
  ((object1
     :initarg :object1)
   (object2
     :initarg :object2)))

(define-condition unexpected-unifier-source (clover-toplevel-condition)
  ((src
     :initarg :src
     :initform nil
     :accessor src-of)))

(define-condition null-clause-not-found (clover-toplevel-condition)
  ())

(define-condition expr-parse-error (clover-toplevel-condition)
  ())

(define-condition multiple-clause-found (clover-toplevel-condition)
  ())

(define-condition unable-to-orient-equation (clover-toplevel-condition)
  ())

(define-condition mkbtt-parse-error (expr-parse-error)
  ())

(defmethod print-object ((error clover-toplevel-condition) stream)
  (format stream "~%~A~%" (message-of error)))

