(defpackage clover.clover
  (:use :cl
        :clover.property
        :clover.search.common
        :clover.search.iddfs
        :clover.search.extractor
        :clover.types
        :clover.resolution
        :clover.util
        )
  (:import-from :clover.unify
                :alphabet=)
  (:export
    :start_resolution
    :find-lemmas
    ))
(in-package :clover.clover)


(defmethod open-nodes ((clause-set clause-set))
  (opener_clause-set clause-set 
                     (clause-set.resolution-mode clause-set)))

(defmethod finish ((clause-set clause-set))
  (some  #'null-clause-p
        (clause-set.clauses clause-set)))

(defmethod node-hash ((node clause-set))
  (sxhash (format nil "~A" node)))

(defmethod node-equality ((node1 clause-set) (node2 clause-set))
  (clause-set= node1 node2))

(defmethod node-same-class ((node1 clause-set) (node2 clause-set))
  (alphabet= node1 node2))


(defmethod start_resolution ((clause-set clause-set))
  (when (some
          (lambda (c) (null (clause.clause-type c)))
          (clause-set.clauses clause-set))
    (error "clause type must not be null"))
  (cond
    (t 
     (default-resolution clause-set))))

(defmethod default-resolution ((clause-set clause-set))
  (let ((clauses (clause-set.clauses clause-set)))
    (loop
      :named exit
      :for raw :in (append (remove-if-not #'conseq-clause-p clauses)
                           (remove-if #'conseq-clause-p clauses))
      :for centerlized-clause := (clause 
                                   (clause.literals raw)
                                   (clause.parent1 raw)
                                   (clause.parent2 raw)
                                   (clause.unifier raw)
                                   :center)
      :for centerlized-clauses := (cons centerlized-clause 
                                           (remove raw
                                                   clauses 
                                                   :test #'clause=))
      :do 
      (multiple-value-bind 
          (cnt value) (iddfs (clause-set centerlized-clauses
                                         :default)
                             *resolution-search-depth*)
        (when cnt
          (return-from exit (values cnt value))))
      )))

