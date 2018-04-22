(defpackage clover
  (:use :cl
        :iddfs
        :types.clover)
  (:export 
        :open-nodes
        :finish))
(in-package :clover)




(defmethod open-nodes ((node clause-set))
  )


(defmethod finish ((node clause-set))
  "節集合中に空節が含まれているか否かを判定する"
  (some (lambda (x)
          (null (expr-set.exprs x))) 
        (clause-set.clauses node)))
