(defpackage clover.clover
  (:use :cl
        :clover.search.iddfs
        :clover.types
        :clover.unify
        )
  (:export 
        :open-nodes
        :finish))
(in-package :clover.clover)



(defmethod open-nodes ((clause-set clause-set))
  )


(defmethod finish ((clause-set clause-set))
  (some (lambda (x)
          (null (clause.literals x))) 
        (clause-set.clauses clause-set)))
