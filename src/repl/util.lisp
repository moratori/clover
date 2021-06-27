(defpackage clover.repl.util
  (:use :cl
        :ppcre
        )
  (:export 
    :%stdout
    :make-bold-string
    :%prompt-toplevel
    :%prompt-def
    :%read-line-with-sigint-guard
    :%parse-input-line
    ))
(in-package :clover.repl.util)


(defmacro %stdout (control-string &rest values)
  `(progn
     (format *standard-output* ,control-string ,@values)
     (force-output *standard-output*)))

(defun make-bold-string (str)
    (format nil "~C[~Am~A~C[0m" 
            (code-char #o33) 
            "1" 
            str
            (code-char #o33)))

(defun %prompt-toplevel (current-axiomatic-system)
  (%stdout 
    (make-bold-string 
      (format nil "(~A)>>> " current-axiomatic-system))))

(defun %prompt-def (counter)
  (%stdout 
    (make-bold-string
      (format nil "axiom[~A]>>> " counter))))

(defun %read-line-with-sigint-guard ()
  (handler-case
      (read-line *standard-input* nil nil)
    (condition (con) 
      (declare (ignore con))
      nil))) 

(defun %parse-input-line (line)
  (multiple-value-bind 
      (match-start match-end reg-starts reg-ends)
      (scan "^:[a-z\\-]+" line)
    (declare (ignore reg-starts reg-ends))
    (let ((command 
            (when match-start
              (subseq line (1+ match-start) match-end)))
          (args
            (when match-start
              (remove-if
                (lambda (x) (zerop (length x)))
                (ppcre:split " " (subseq line match-end))))))
      (cond 
        ((string= line "")
         (values :SKIPP (list line)))
        ((null match-start)
         (values :DEFAULT (list line)))
        (t 
         (values
           (intern (string-upcase command) (find-package "KEYWORD"))
           args))))))
