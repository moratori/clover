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
    (clause.unifier clause)))


(defmethod %remove-law-of-exclude-middle ((clause clause))
  (let ((literals (clause.literals clause)))
    (clause
      (remove-if
        (lambda (lit)
          (some
            (lambda (target)
              (complement-literal-p target lit))
            literals))
        literals)
      (clause.parent1  clause)
      (clause.parent2  clause)
      (clause.focus-literal clause)
      (clause.unifier clause))))


(defmethod simplify ((clause clause))
  (%remove-duplicates-literal
    (%remove-law-of-exclude-middle 
      clause)))

(defmethod simplify ((clause-set clause-set))
  (clause-set
    (remove-duplicates
      (mapcar 
        #'simplify
        (clause-set.clauses clause-set))
      :test #'clause=)))
