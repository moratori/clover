(defpackage clover.core
  (:use :cl
        :iddfs
        :clover.types
        :clover.unify
        )
  (:export 
        :open-nodes
        :finish))
(in-package :clover.core)



(defmethod open-nodes ((clause-set clause-set))
  )


(defmethod finish ((clause-set clause-set))
  (some (lambda (x)
          (null (clause.exprs x))) 
        (clause-set.clauses clause-set)))
