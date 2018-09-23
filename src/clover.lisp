(defpackage clover.clover
  (:use :cl
        :clover.property
        :clover.search.iddfs
        :clover.types
        :clover.resolution
        :clover.util
        )
  (:export
    :start_resolution
    ))
(in-package :clover.clover)


(defmethod open-nodes ((clause-set clause-set))
  (opener_clause-set clause-set *resolution-algorithm*))


(defmethod finish ((clause-set clause-set))
  (some  #'null-clause-p
        (clause-set.clauses clause-set)))


(defmethod start_resolution ((clause-set clause-set))
  (iddfs clause-set *resolution-search-depth*))
