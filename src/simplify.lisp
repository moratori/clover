(defpackage clover.simplify
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        )
  (:export
    :simplify
    )
  )
(in-package :clover.simplify)


(defmethod %remove-duplicates-literal ((clause clause))
  (clause
    (remove-duplicates
      (clause.literals clause)
      :test #'literal=)
    (clause.parent1  clause)
    (clause.parent2  clause)
    (clause.focus-literal clause)
    (clause.unifier clause)
    (clause.clause-type clause)))


(defmethod %include-law-of-exclude-middle-p ((clause clause))
  (let ((literals (clause.literals clause)))
    (some 
      (lambda (lit1)
        (some 
          (lambda (lit2)
            (complement-literal-p lit1 lit2))
          literals))
      literals)))


(defmethod simplify ((clause clause))
  (%remove-duplicates-literal clause))

(defmethod simplify ((clause-set clause-set))
  (clause-set
    (remove-duplicates
      (remove-if
        #'%include-law-of-exclude-middle-p
        (mapcar 
          #'simplify
          (clause-set.clauses clause-set)))
      :test #'clause=)))
