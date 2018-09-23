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
    :opener_exhaustive-worst-resolution
    )
  )
(in-package :clover.resolution)



(defmethod %resolution ((clause1 clause) (clause2 clause))
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
                        :collect
                        (clause 
                          (append 
                            (remove-if 
                              (lambda (l) (literal= target-literal l))
                              res-clause-left-literals)
                            (remove-if 
                              (lambda (l) (complement-literal-p target-literal l))
                              res-clause-right-literals))
                          (when *save-resolution-history* clause1)
                          (when *save-resolution-history* clause2)
                          (when *save-resolution-history* literal1)
                          (when *save-resolution-history* unifier-set))))))


(defmethod %prepare-resolution ((clause-set clause-set))
  "導出処理に先立ち、節集合をきれいにする"
  (simplify
    (rename clause-set)))



(defmethod opener_exhaustive-worst-resolution ((clause-set clause-set))
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
    (mapcar 
      (lambda (clause)
        (clause-set
          (cons clause clauses)))
      next-each-nodes)))


