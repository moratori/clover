(defpackage clover.rendertree
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        )
  (:export
    :render-refutation-tree
    )
  )
(in-package :clover.rendertree)


(defmethod %collect-graphviz-node-and-edges ((clause clause) seq)
  (let ((unifier (clause.unifier clause))
        (parent1 (clause.parent1 clause))
        (parent2 (clause.parent2 clause))
        (myself  (list (format nil "CLAUSE_~A[label = \"~A\"];" seq clause))))
    (cond 
      ((or (null parent1)
           (null parent2))
       (values myself nil))
      (t 
       (let* ((left-seq (* seq 2))
              (right-seq (1+ left-seq))
              (parent1-nodes-edges 
                (multiple-value-list (%collect-graphviz-node-and-edges parent1 left-seq)))
              (parent2-nodes-edges 
                (multiple-value-list (%collect-graphviz-node-and-edges parent2 right-seq)))
              (parent1-nodes (first parent1-nodes-edges))
              (parent1-edges (second parent1-nodes-edges))
              (parent2-nodes (first parent2-nodes-edges))
              (parent2-edges (second parent2-nodes-edges))
              (neighbor-edges
                (list 
                  (format nil "CLAUSE_~A -> CLAUSE_~A[label = \"~A\"];" left-seq seq unifier)
                  (format nil "CLAUSE_~A -> CLAUSE_~A[label = \"~A\"];" right-seq seq unifier))))
         (values 
           (append parent1-nodes myself parent2-nodes)
           (append parent1-edges neighbor-edges parent2-edges)))))))


(defmethod render-refutation-tree ((clause-set clause-set) (filepath pathname))
  (let ((target-clause
          (find-if 
            #'null-clause-p
            (clause-set.clauses clause-set))))

    (unless target-clause
      (error 
        (make-condition 'null-clause-not-found
                        :message "null clause not found for redering refutation tree")))

    (multiple-value-bind (nodes edges) 
        (%collect-graphviz-node-and-edges target-clause 1)

      (with-open-file (stream filepath :direction :output :if-exists :supersede)
        (format stream "digraph refutation_tree {~%")
        (format stream "~{  ~A~%~}~%" nodes)
        (format stream "~{  ~A~%~}~%" edges)
        (format stream "}")))))


