(defpackage clover.resolution
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.simplify
        :clover.rename
        :clover.substitute
        )
  (:export
    :%collect-resolutable-literal
    :%resolution
    :opener_clause-set
    )
  )
(in-package :clover.resolution)



(defmethod %resolution ((clause1 clause) (clause2 clause) &optional 
                                         (get-clause-type (lambda (c) :resolvent)))
  (let ((literals1 (clause.literals clause1))
        (literals2 (clause.literals clause2)))
    (loop :for literal1 :in literals1
          :for unifier-set-list := 
               (loop :for literal2 :in literals2
                     :for us := (handler-case 
                                    (find-most-general-unifier-set
                                      literal1 literal2)
                                  (ununifiable-literal-error (e) nil))
                     :if us 
                     :collect us)
          :if unifier-set-list 
          :append (loop :for unifier-set :in unifier-set-list
                        :for res-clause-left  = (apply-unifier-set clause1 unifier-set)
                        :for res-clause-left-literals = (clause.literals res-clause-left)
                        :for res-clause-right = (apply-unifier-set clause2 unifier-set)
                        :for res-clause-right-literals = (clause.literals res-clause-right)
                        :for target-literal   = (apply-unifier-set literal1 unifier-set)
                        :for resolvent = (append (remove-if 
                                                   (lambda (l) (literal= target-literal l))
                                                   res-clause-left-literals)
                                                 (remove-if 
                                                   (lambda (l) (complement-literal-p target-literal l))
                                                   res-clause-right-literals))
                        :collect
                        (clause 
                          resolvent
                          (when *save-resolution-history* clause1)
                          (when *save-resolution-history* clause2)
                          (when *save-resolution-history* literal1)
                          (when *save-resolution-history* unifier-set)
                          (funcall get-clause-type resolvent))))))


(defmethod %prepare-resolution ((clause-set clause-set))
  "導出処理に先立ち、節集合をきれいにする"
  (simplify
    (rename clause-set)))


(defun %sort-clause-set-list (clause-set-list)
  (sort 
    clause-set-list
    (lambda (clause-set1 clause-set2)
      (<
        (apply #'min (mapcar #'clause-length (clause-set.clauses clause-set1)))
        (apply #'min (mapcar #'clause-length (clause-set.clauses clause-set2)))))))



(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :exhaustive)))
  (let* ((prepared 
           (%prepare-resolution clause-set))
         (clauses
           (clause-set.clauses prepared))
         (next-each-nodes
           (loop :for clause1 :in clauses
                 :for i :from 0 
                 :append
                 (loop :for clause2 :in clauses
                       :for j :from 0
                       :if (> j i)
                       :append (%resolution clause1 clause2)))))
    (%sort-clause-set-list
     (mapcar 
      (lambda (clause)
        (clause-set
          (cons clause clauses)))
      next-each-nodes))))

(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :linear)))
  "clause-type が :goal となっているもののみに対して、ノード展開する
   - clause-set中に :goal 節が複数ある場合は、エラー
   - :goal 以外の節で既に充足不可能な様な場合は、探索の停止性は保証されない"
  (let* ((prepared (%prepare-resolution clause-set))
         (clauses (clause-set.clauses prepared))
         (goal-clause-count 
           (count-if 
             (lambda (c)
               (eq (clause.clause-type c) :goal))
             clauses))
         (goal-clause
           (find-if 
             (lambda (c)
               (eq (clause.clause-type c) :goal))
             clauses)))
    (when (= goal-clause-count 1)
      
      (let ((next-base-clauses
              (cons 
                (clause 
                  (clause.literals goal-clause)
                  (clause.parent1 goal-clause)
                  (clause.parent2 goal-clause)
                  (clause.focus-literal goal-clause)
                  (clause.unifier goal-clause)
                  :resolvent)
                (remove-if 
                  (lambda (c)
                    (eq (clause.clause-type c) :goal))
                  clauses)))
            (next-each-nodes
              (loop :for clause :in clauses
                    :if (not (clause= clause goal-clause))
                    :append (%resolution goal-clause clause 
                                         (lambda (x) :goal)))))
        (%sort-clause-set-list
         (mapcar
          (lambda (c)
            (clause-set
              (cons c next-base-clauses)))
          next-each-nodes))))))


