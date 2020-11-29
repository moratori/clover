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
  (:import-from :alexandria
                :shuffle)
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
                     :if (and us (not (eq (literal.negation literal1)
                                          (literal.negation literal2))))
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
                          (when *save-resolution-history* unifier-set)
                          (funcall get-clause-type resolvent))))))


(defmethod opener_clause-set :around ((clause-set clause-set) resolution-mode)
  (call-next-method 
    (simplify
      (rename clause-set))
    resolution-mode))

(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :exhaustive)))
  (let* ((clauses
           (clause-set.clauses clause-set))
         (next-each-nodes
           (loop :for clause1 :in clauses
                 :for i :from 0 
                 :append
                 (loop :for clause2 :in clauses
                       :for j :from 0
                       :if (> j i)
                       :append (%resolution clause1 clause2)))))
    (sort-clause-set-list-short-to-long
     (mapcar 
      (lambda (clause)
        (clause-set
          (cons clause clauses)
          resolution-mode))
      next-each-nodes))))

(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :horn)))
  (let* ((clauses (clause-set.clauses clause-set))
         (goal-clause
           (find-if #'goal-clause-p clauses)))
    (when goal-clause 
      (let ((next-base-clauses
              (cons 
                (clause 
                  (clause.literals goal-clause)
                  (clause.parent1 goal-clause)
                  (clause.parent2 goal-clause)
                  (clause.unifier goal-clause)
                  :resolvent)
                (remove goal-clause
                        clauses :test #'clause=)))
            (next-each-clauses
              (loop :for clause :in clauses
                    :if (not (clause= clause goal-clause))
                    :append (%resolution goal-clause clause))))
        (sort-clause-set-list-short-to-long
         (mapcar
          (lambda (c)
            (clause-set
              (cons c next-base-clauses)
              resolution-mode))
          next-each-clauses))))))

(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :default)))
  (let* ((clauses (clause-set.clauses clause-set))
         (center-clause
           (find-if 
             (lambda (c)
               (eq (clause.clause-type c) :center))
             clauses)))
    (when center-clause
      (let ((next-base-clauses
              (cons 
                (clause 
                  (clause.literals center-clause)
                  (clause.parent1 center-clause)
                  (clause.parent2 center-clause)
                  (clause.unifier center-clause)
                  :resolvent)
                (remove center-clause
                        clauses :test #'clause=)))
            (next-each-clauses
              (loop :for clause :in clauses
                    :if (not (clause= clause center-clause))
                    :append (%resolution center-clause clause 
                                         (lambda (x) :center)))))
        (sort-clause-set-list-short-to-long
         (mapcar
          (lambda (c)
            (clause-set
              (cons c next-base-clauses)
              resolution-mode))
          next-each-clauses))))))

(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :precipitately)))
  (let* ((clauses (clause-set.clauses clause-set))
         (center-clause
           (find-if 
             (lambda (c)
               (eq (clause.clause-type c) :center))
             clauses)))
    (when center-clause
      (let ((next-base-clauses
              (cons 
                (clause 
                  (clause.literals center-clause)
                  (clause.parent1 center-clause)
                  (clause.parent2 center-clause)
                  (clause.unifier center-clause)
                  :resolvent)
                (remove center-clause
                        clauses :test #'clause=)))
            (next-each-clauses
              ;; 始めに導出できた節の事しか考えない
              (loop :named exit
                    :for clause :in (shuffle clauses) 
                    ;; don't use `clauses` following 
                    ;; shuffle is destructive function
                    :for res := (%resolution center-clause clause 
                                         (lambda (x) :center))
                    :if (and (not (clause= clause center-clause))
                             (not (null res)))
                    :do (return-from exit res))))
        (sort-clause-set-list-short-to-long
         (mapcar
          (lambda (c)
            (clause-set
              (cons c next-base-clauses)
              resolution-mode))
          next-each-clauses))))))

