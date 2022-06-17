(defpackage clover.clover
  (:use :cl
        :clover.property
        :clover.search.common
        :clover.search.iddfs
        :clover.search.astar
        :clover.search.extractor
        :clover.types
        :clover.resolution
        :clover.util
        )
  (:import-from :alexandria
                :median
                :compose)
  (:import-from :clover.unify
                :alphabet=)
  (:import-from :clover.rewrite
                :rewrite-final)
  (:export
    :start_resolution
    :start_trs
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

(defmethod cost-to-goal ((node clause-set))
  (loop
    :for clause :in (clause-set.clauses node)
    :minimize (length (clause.literals clause))))

(defmethod cost-to-neighbor ((node1 clause-set) (node2 clause-set))
  (let ((clauses (clause-set.clauses node2)))
    (if clauses
        (*
          (length clauses)
          (median 
            (mapcar 
              (compose #'length #'clause.literals) 
              clauses)))
        1)))

(defmethod start_trs ((expr equation) (rewrite-rule-set rewrite-rule-set))
  (let* ((left (equation.left expr))
         (right (equation.right expr))
         (negation (equation.negation expr))
         (final-left (rewrite-final left rewrite-rule-set))
         (final-right (rewrite-final right rewrite-rule-set)))
    (if negation
        (values (term/= final-left final-right)
                (equation negation final-left final-right))
        (values (term= final-left final-right)
                (equation negation final-left final-right)))))

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
          (foundp value) (astar (clause-set centerlized-clauses
                                         :default))
        (when foundp
          (return-from exit (values foundp value)))))))

