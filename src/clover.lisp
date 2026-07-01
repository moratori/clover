(defpackage clover.clover
  (:use :cl
        :clover.parameters
        :clover.search.common
        :clover.search.astar
        :clover.search.iddfs
        :clover.search.dfs
        :clover.types
        :clover.resolution
        :clover.logical-predicates
        )
  (:import-from :clover.equality
                :term=
                :term/=
                :clause=)
  (:import-from :alexandria
                :median
                :variance)
  (:import-from :clover.unify
                :alphabet-equivalent-p)
  (:import-from :clover.canonicalization
                :canonical-clause-string)
  (:import-from :clover.rewrite
                :rewrite-final)
  (:import-from :clover.multiprocess
                :initialize-lparallel-kernel)
  (:export
    :start_resolution
    :start_trs
    ))
(in-package :clover.clover)


(defmethod open-nodes ((clause-set clause-set))
  (opener_clause-set clause-set 
                     (clause-set.resolution-mode clause-set)))

(defmethod finish ((clause-set clause-set))
  (some  #'null-clause-p
        (clause-set.clauses clause-set)))

(defmethod node-canonical-key ((node clause-set))
  ;; α同値(厳密な変種)な clause-set を同一キー化する正準キー文字列。
  ;; 各節を変数初出順で1パス直列化し、節文字列をソートして連結(clause-set は節順非依存のため)。
  ;; これは alphabet-equivalent-p の過小近似(リテラル内順序や condensation は畳まない)であり、非等価な状態を
  ;; 誤って同一視しない=closed集合による枝刈りは健全。畳み損ねたぶんは重複排除が減るだけ。
  ;; 節の直列化は clover.canonicalization:canonical-clause-string に分離している。
  (format nil "~{~A~^/~}"
    (sort
      (mapcar #'canonical-clause-string (clause-set.clauses node))
      #'string<)))

(defmethod cost-to-goal ((node clause-set))
  (loop
    :for clause :in (clause-set.clauses node)
    :minimize (clause-length clause)))

(defmethod cost-to-neighbor ((node1 clause-set) (node2 clause-set))
  (let ((clauses (clause-set.clauses node2)))
    (if clauses
        (*
          (length clauses)
          (median 
            (mapcar 
              #'clause-length
              clauses))
          (1+ (variance
                (mapcar #'clause.used-cnt clauses))))
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
        (t :default)))))

(defmethod start_resolution ((clause-set clause-set))

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

  (let* ((target
           (prepare-resolution clause-set))
         (available-search 
           (list 
             #'astar))
         (result
           (some
             (lambda (fn)
               (multiple-value-bind (foundp node)
                   (funcall fn target)
                 (when foundp
                   (list foundp node))))
             available-search)))
    (if result
        (values-list result)
        (values nil nil))))

