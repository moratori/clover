(defpackage clover.resolution
  (:use :cl
        :clover.parameters
        :clover.conditions
        :clover.types 
        :clover.logical-predicates
        :clover.unify
        :clover.simplify
        :clover.rename
        :clover.substitute
        )
  (:import-from :clover.lib.util
                :pairwise-collect-if)
  (:import-from :clover.equality
                :literal=
                :clause=)
  (:import-from :alexandria
                :shuffle)
  (:export
    :resolution
    :resolution-wrapper
    :prepare-resolution
    :opener_clause-set
    )
  )
(in-package :clover.resolution)



(defmethod resolution ((parent1 clause) (parent2 clause) (resolution-mode (eql :default)) 
                                        (get-resolvent-type function)
                                        (get-parent1-type function)
                                        (get-parent2-type function))
  (let* ((literals1 (clause.literals parent1))
         (literals2 (clause.literals parent2))
         (new-parent1
           (clause
             (clause.literals parent1)
             (clause.parent1 parent1)
             (clause.parent2 parent1)
             (clause.unifier parent1)
             (funcall get-parent1-type parent1)
             (1+ (clause.used-cnt parent1))))
         (new-parent2
           (clause
             (clause.literals parent2)
             (clause.parent1 parent2)
             (clause.parent2 parent2)
             (clause.unifier parent2)
             (funcall get-parent2-type parent2)
             (1+ (clause.used-cnt parent2))))
         (all-resolvents-from-parents
           (loop :for literal1 :in literals1
                 :for unifier-set-list := 
                      (loop :for literal2 :in literals2
                            :for us := (handler-case 
                                           (find-most-general-unifier-set
                                             literal1 literal2)
                                         (ununifiable-error (e) nil))
                            :if (and us (not (eq (literal.negation literal1)
                                                 (literal.negation literal2))))
                            :collect us)
                 :if unifier-set-list 
                 :append 
                 (loop :for unifier-set :in unifier-set-list
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
                         (funcall get-resolvent-type resolvent))))))
    (values
      new-parent1
      new-parent2
      all-resolvents-from-parents)))


(defmethod resolution ((parent1 clause) (parent2 clause) (resolution-mode (eql :snl)) 
                                        (get-resolvent-type function)
                                        (get-parent1-type function)
                                        (get-parent2-type function))

  (assert (and (goal-clause-p parent1)
               (or (rule-clause-p parent2)
                   (fact-clause-p parent2))))

  (let* ((literals1 (clause.literals parent1))
         (literals2 (clause.literals parent2))
         (last-literal (first (last literals1)))
         (last-literal-negation (literal.negation last-literal))
         ; last-literalと相補的な関係になるのは、literals2中に1つしかない
         (opposite 
           (find-if (lambda (x) (eq nil (literal.negation x))) 
                    literals2))
         (us
           (handler-case 
               (find-most-general-unifier-set
                 last-literal opposite)
             (ununifiable-error (e) nil)))
         (result
           (when us
             (let* ((res-clause-left (apply-unifier-set parent1 us))
                    (res-clause-left-literals (clause.literals res-clause-left))
                    (res-clause-right (apply-unifier-set parent2 us))
                    (res-clause-right-literals (clause.literals res-clause-right))
                    (target-literal (apply-unifier-set last-literal us))
                    (resolvent (append (remove target-literal res-clause-left-literals
                                               :test #'literal=)
                                       (remove target-literal res-clause-right-literals
                                               :test #'complement-literal-p))))
               (clause 
                 resolvent
                 (when *save-resolution-history* parent1)
                 (when *save-resolution-history* parent2)
                 (when *save-resolution-history* us)
                 (funcall get-resolvent-type resolvent))))))
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
      (when result (list result)))))


(defmethod factoring ((clause clause) (resolution-mode (eql :snl)) (factor-type function) (target-type function))
  (values clause nil))

(defmethod factoring ((clause clause) (resolution-mode (eql :default)) (factor-type function) (target-type function))
  (let*  ((literals
           (clause.literals clause))
          (all-factors
            (remove-duplicates
              (pairwise-collect-if
                (lambda (l1 l2)
                  (let ((mgu 
                          (handler-case 
                              (find-most-general-unifier-set
                                l1 l2)
                            (ununifiable-error (e) nil))))
                    (cond
                      ((null mgu) (values nil nil))
                      ((not (eq (literal.negation l1) (literal.negation l2))) (values nil nil))
                      (t
                       (values 
                         t
                         (let ((new-literals
                                 (remove-duplicates
                                   (mapcar 
                                     (lambda (l)
                                       (apply-unifier-set l mgu)) literals)
                                   :test #'literal=)))
                           (clause
                             new-literals
                             (when *save-resolution-history* clause) ;; 導出とは異なり、単一の節からfactorが生成されるため
                             (when *save-resolution-history* clause) ;; 便宜上、左右の親として同一の節を設定する。
                             (when *save-resolution-history* mgu)
                             (funcall factor-type clause))))))))
                literals)
              :test #'alphabet-equivalent-p)))
    (values
      (clause
        (clause.literals clause)
        (clause.parent1 clause)
        (clause.parent2 clause)
        (clause.unifier clause)
        (funcall target-type clause)
        (1+ (clause.used-cnt clause)))
      all-factors)))


(defmethod factoring-wrapper ((clause-set clause-set) (clause clause) resolution-mode factor-type target-type)
  (let ((base-clauses 
          (remove
            clause
            (clause-set.clauses clause-set)
            :test #'clause=))) 
    (multiple-value-bind
        (org factors)
        (factoring clause resolution-mode factor-type target-type)
      (mapcar
        (lambda (clause)
          (clause-set
            (append (list clause)
                    base-clauses
                    (list org))
            resolution-mode))
        factors))))



; center節に限定せず、clause-set の各節から、作成可能なすべての因子を元に新たな節集合を生成するもの
;(defmethod factoring-wrapper ((clause-set clause-set) (clause clause) resolution-mode factor-type target-type)
;  (let* ((clauses (clause-set.clauses clause-set)))
;    (loop
;      :for c :in clauses
;      :for factored := (factoring c resolution-mode)
;      :append
;      (loop
;        :for each :in factored
;        :collect
;        (clause-set
;          (cons each clauses)
;          (clause-set.resolution-mode clause-set))))))




(defmethod resolution-wrapper ((clause-set clause-set) 
                                    (parent1 clause) 
                                    (parent2 clause)
                                    resolution-mode
                                    (resolvent-type function)
                                    (parent1-type function)
                                    (parent2-type function))
  (let ((base-clauses 
          (remove-if 
            (lambda (clause)
              (or (clause= clause parent1)
                  (clause= clause parent2)))
            (clause-set.clauses clause-set)))) 
    (multiple-value-bind
        (new-parent1 new-parent2 resoluted-clauses)
        (resolution parent1 parent2 resolution-mode resolvent-type parent1-type parent2-type)
      (mapcar
        (lambda (clause)
          (clause-set
            (append (list clause new-parent2)
                    base-clauses
                    (list new-parent1))
            resolution-mode))
        resoluted-clauses))))

(defmethod prepare-resolution ((clause-set clause-set))
  "頂節とresolution-modeを決定し、clause-setを返却する"
  (let* ((clauses
           (clause-set.clauses clause-set))
         (conseq
           (find-if (lambda (clause) 
                      (eq :conseq (clause.clause-type clause)))
                    clauses))
         (base-clauses
           (progn
             (when (null conseq)
               (error "consequent clause is required"))
             (remove conseq clauses :test #'clause=)))
         (centerlized-clause
           (clause 
             (clause.literals conseq)
             (clause.parent1 conseq)
             (clause.parent2 conseq)
             (clause.unifier conseq)
             :center)))
    (when (some
            (lambda (c) (null (clause.clause-type c)))
            (clause-set.clauses clause-set))
      (error "clause type must not be null"))

    (when (< 1 
             (count-if 
               (lambda (clause)
                 (eq (clause.clause-type clause) :conseq))
               (clause-set.clauses clause-set)))
      (error "multiple consequence clause found")) 

    (full-simplify
      (rename
        (clause-set
          (cons centerlized-clause base-clauses)
          (cond
            ((and (every 
                    (lambda (c)
                      (or (fact-clause-p c) 
                          (rule-clause-p c)))
                    base-clauses)
                  (goal-clause-p conseq))
             :snl)
            (t :default)))))))




(defmethod opener_clause-set :around ((clause-set clause-set) resolution-mode)
  (call-next-method 
    (incremental-simplify
      (rename clause-set))
    resolution-mode))


(defmethod opener_clause-set :before ((clause-set clause-set) resolution-mode)
  (when (< 1 (count-if 
               (lambda (c) 
                 (eq (clause.clause-type c) :center))
               (clause-set.clauses clause-set)))
    (error (make-condition 'multiple-clause-found
                           :message ":center"))))


(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :default)))
  (let* ((clauses 
           (clause-set.clauses clause-set))
         (center-clause
           (find-if 
             (lambda (c) (eq (clause.clause-type c) :center))
             clauses))) 
    (when center-clause
      (let ((generated-by-resolution
              (loop
                :for clause :in clauses
                :for clause-type := (clause.clause-type clause)
                :unless (eq clause-type :center)
                :append 
                (resolution-wrapper
                  clause-set
                  center-clause
                  clause
                  resolution-mode
                  (lambda (x) :center)
                  (lambda (x) :resolvent)
                  (lambda (x) clause-type))))
            (generated-by-factoring
              (factoring-wrapper
                clause-set
                center-clause
                resolution-mode
                (lambda (x) :center)
                (lambda (x) :resolvent))))
        (append
          generated-by-resolution
          generated-by-factoring)))))


(defmethod opener_clause-set ((clause-set clause-set) (resolution-mode (eql :snl)))
  (let* ((clauses 
           (clause-set.clauses clause-set))
         (center-clause
           (find-if 
             (lambda (c) (eq (clause.clause-type c) :center))
             clauses))) 
    (when center-clause
      (loop
        :for clause :in clauses
        :for clause-type := (clause.clause-type clause)
        :if (eq clause-type :premise)
        :append
        (resolution-wrapper
          clause-set
          center-clause
          clause
          resolution-mode
          (lambda (x) :center)
          (lambda (x) :resolvent)
          (lambda (x) :premise))))))

