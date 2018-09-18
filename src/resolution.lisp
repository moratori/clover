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
    )
  )
(in-package :clover.resolution)



(defmethod %collect-resolutable-literal ((clause1 clause) (clause2 clause))
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
          :collect (resolution-candidate literal1 unifier-set-list))))


(defmethod %resolution ((clause1 clause) (clause2 clause))
  "clause1 と clause2を導出することで得ることの可能な節clauseの全てをリストで得る"
  (let ((res-clause (clause (append (clause.literals clause1)
                                    (clause.literals clause2)))))
    (mapcan
      (lambda (rc)
        (let ((focus-literal    (res-cand.focus-literal rc))
              (unifier-set-list (res-cand.cand-unifiers rc)))
          (mapcar 
            (lambda (us)
              (let ((unifiered-focus-literal 
                      (apply-unifier-set focus-literal us))
                    (unifiered-clause
                      (apply-unifier-set res-clause us)))
                (clause
                  (remove-if
                    (lambda (lit)
                      (or 
                        (literal= unifiered-focus-literal lit)
                        (complement-literal-p unifiered-focus-literal lit)))
                    (clause.literals unifiered-clause))
                  (when *save-resolution-history* clause1)
                  (when *save-resolution-history* clause2)
                  (when *save-resolution-history* focus-literal)
                  (when *save-resolution-history* us))))
            unifier-set-list)))
      (%collect-resolutable-literal
        clause1
        clause2))))


(defmethod %prepare-resolution ((clause-set clause-set))
  (rename
    (simplify clause-set)))



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


