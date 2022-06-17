(defpackage clover.rendertree
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        )
  (:import-from :clover.rename
                :rename-for-human-readable-printing)
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
                  (format nil "CLAUSE_~A -> CLAUSE_~A;" right-seq seq))))
         (values 
           (append parent1-nodes myself parent2-nodes)
           (append parent1-edges neighbor-edges parent2-edges)))))))

(defmethod %check-renderable-to-terminal ((clause clause))
  (let* ((parent1 (clause.parent1 clause))
         (parent2 (clause.parent2 clause)))
    (cond
      ((not (has-parent-p clause)) t)
      ((not (has-parent-p parent1))
       (%check-renderable-to-terminal parent2))
      ((not (has-parent-p parent2))
       (%check-renderable-to-terminal parent1))
      (t nil))))

(defmethod %render-refutation-tree-terminal ((clause clause) (output stream))
  (let* ((parent1 (clause.parent1 clause))
         (parent2 (clause.parent2 clause))
         (right (cond
                 ((and parent1 (not (has-parent-p parent1)))
                  parent1)
                 ((and parent2 (not (has-parent-p parent2)))
                  parent2)
                 (t nil)))
         (down (cond
                  ((and parent1 (has-parent-p parent1))
                   parent1)
                  ((and parent2 (has-parent-p parent2))
                   parent2)
                  ((and parent1 parent2)
                   (find-if 
                     (lambda (p) (not (clause= p right)))
                     (list parent1 parent2)))
                  (t nil)))
         (clause-for-printing
           (rename-for-human-readable-printing clause))
         (right-for-printing
           (when right
             (rename-for-human-readable-printing right))))
    (cond
      ((and (null right) (null down))
       (format output " ~A~%" clause-for-printing))
      (t 
       (let* ((space-size 
                (floor (/ (length (format nil "~A" clause-for-printing)) 2)))
              (pad
                (loop :for i :from 0 :to space-size :collect " ")))
         (format output " ~A ←← ~A~%" clause-for-printing right-for-printing)
         (format output "~{~A~}↑~%" pad)
         (format output "~{~A~}↑~%" pad))
       (%render-refutation-tree-terminal down output)))))

(defmethod render-refutation-tree ((clause-set clause-set) (output stream))
  ;; 証明できた時に、画面に出力する為のメソッド
  ;; 線形探索でない場合の証明図は、端末に表示するのが困難であるため出力しない
  (let* ((target-clause
           (find-if 
             #'null-clause-p
             (clause-set.clauses clause-set))))

    (unless target-clause
      (error 
        (make-condition 'null-clause-not-found
                        :message "null clause not found for redering refutation tree")))
    (when (%check-renderable-to-terminal target-clause)
      (%render-refutation-tree-terminal target-clause output)))) 


(defmethod render-refutation-tree ((clause-set clause-set) (output pathname))
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

      (handler-case
          (with-open-file (handle output :direction :output :if-exists :supersede)
            (format handle "digraph refutation_tree {~%")
            (format handle "~{  ~A~%~}~%" nodes)
            (format handle "~{  ~A~%~}~%" edges)
            (format handle "}"))
        (condition (con)
          (declare (ignore con))
          (format *standard-output* "unable to write file: ~A~%" output))))))

