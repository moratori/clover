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
    :comprehensive-resolvent
    )
  )
(in-package :clover.resolution)



(defmethod %resolution ((parent1 clause) (parent2 clause) &optional 
                                         (get-resolvent-type (lambda (c) :resolvent))
                                         (get-parent1-type (lambda (c) :resolvent))
                                         (get-parent2-type (lambda (c) :resolvent)))
  (let ((literals1 (clause.literals parent1))
        (literals2 (clause.literals parent2)))
    (values
      (clause
        (clause.literals parent1)
        (clause.parent1 parent1)
        (clause.parent2 parent1)
        (clause.unifier parent1)
        (funcall get-parent1-type parent1)
        (1+ (clause.used-cnt parent1)))
      (clause
        (clause.literals parent2)
        (clause.parent1 parent2)
        (clause.parent2 parent2)
        (clause.unifier parent2)
        (funcall get-parent2-type parent2)
        (1+ (clause.used-cnt parent2)))
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
                          :for res-clause-left  := (apply-unifier-set parent1 unifier-set)
                          :for res-clause-left-literals := (clause.literals res-clause-left)
                          :for res-clause-right := (apply-unifier-set parent2 unifier-set)
                          :for res-clause-right-literals := (clause.literals res-clause-right)
                          :for target-literal   := (apply-unifier-set literal1 unifier-set)
                          :for resolvent := (append 
                                              (remove
                                                target-literal
                                                res-clause-left-literals
                                                :test #'literal=)
                                              (remove
                                                target-literal
                                                res-clause-right-literals
                                                :test #'complement-literal-p))
                          :collect
                          (clause 
                            resolvent
                            (when *save-resolution-history* parent1)
                            (when *save-resolution-history* parent2)
                            (when *save-resolution-history* unifier-set)
                            (funcall get-resolvent-type resolvent)))))))


(defmethod comprehensive-resolvent ((clause-set clause-set) 
                                    (parent1 clause) 
                                    (parent2 clause)
                                    resolution-mode
                                    (resolvent-type function)
                                    (parent1-type function)
                                    (parent2-type function))
  "節parent1,parent2の導出結果(resolvent)を網羅的に計算し、
   各resolventをclause-setに追加して返却する"
  (let ((base-clauses 
          (remove-if 
            (lambda (clause)
              (or (clause= clause parent1)
                  (clause= clause parent2)))
            (clause-set.clauses clause-set)))) 
    (multiple-value-bind
        (new-parent1 new-parent2 resoluted-clauses)
        (%resolution parent1 parent2 resolvent-type parent1-type parent2-type)
      (mapcar
        (lambda (clause)
          (clause-set
            (append base-clauses
                    (list clause new-parent1 new-parent2))
            resolution-mode))
        resoluted-clauses))))


(defmethod opener_clause-set :around ((clause-set clause-set) resolution-mode)
  (call-next-method 
    (simplify
      (rename clause-set))
    resolution-mode))



(defmethod opener_clause-set :before ((clause-set clause-set) (resolution-mode (eql :linear)))
  (when (< 1 (count-if 
               (lambda (c) 
                 (eq (clause.clause-type c) :center))
               (clause-set.clauses clause-set)))
    (error (make-condition 'multiple-clause-found
                           :message ":center"))))


(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :linear)))
  (let* ((clauses 
           (clause-set.clauses clause-set))
         (center-clause
           (find-if 
             (lambda (c) (eq (clause.clause-type c) :center))
             clauses))) 
    (when center-clause
          (loop
            :named exit
            :for clause :in clauses
            :for clause-type := (clause.clause-type clause)
            :for ret := (comprehensive-resolvent
                          clause-set
                          center-clause
                          clause
                          resolution-mode
                          (lambda (x) :center)
                          (lambda (x) :resolvent)
                          (lambda (x) clause-type))
            :if (not (eq clause-type :center))
            :append ret))))

