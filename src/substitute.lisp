(defpackage clover.substitute
  (:use :cl
        :iddfs
        :clover.conditions
        :clover.types
        :clover.util
        )
  (:export
    :apply-unifier
    :apply-unifier-set
    )
  )
(in-package :clover.substitute)



(defmethod apply-unifier ((vterm vterm) (unifier unifier))
  (let ((src (unifier.src unifier))
        (dst (unifier.dst unifier)))
    (if (term= src vterm)
      dst
      vterm)))

(defmethod apply-unifier ((fterm fterm) (unifier unifier))
  (fterm
    (fterm.fsymbol fterm)
    (mapcar 
      (lambda (arg)
        (apply-unifier arg unifier))
      (fterm.args fterm))))

(defmethod apply-unifier ((literal literal) (unifier unifier))
  (literal
    (literal.negation literal)
    (literal.predicate literal)
    (mapcar 
      (lambda (arg)
        (apply-unifier arg unifier))
      (literal.args literal))))

(defmethod apply-unifier ((clause clause) (unifier unifier))
  (clause 
    (mapcar
      (lambda (literal)
        (apply-unifier literal unifier))
      (clause.literals clause))))

(defmethod apply-unifier ((target unifier) (unifier unifier))
  (let ((src (apply-unifier (unifier.src target) unifier))
        (dst (apply-unifier (unifier.dst target) unifier)))
    (when (typep src 'fterm)
      (error
        (make-condition 'unexpected-unifier-source
                        :message "source of unifier is fterm"
                        :src src)))
    (unifier src dst)))



(defmethod apply-unifier-set ((term term) (unifier-set unifier-set))
  (reduce 
    #'apply-unifier 
    (unifier-set.unifiers unifier-set)
    :initial-value term))

(defmethod apply-unifier-set ((logical-expression logical-expression) (unifier-set unifier-set))
  (reduce 
    #'apply-unifier
    (unifier-set.unifiers unifier-set)
    :initial-value logical-expression))

