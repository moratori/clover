(defpackage clover.clover
  (:use :cl
        :clover.property
        :clover.search.common
        :clover.search.astar
        :clover.search.iddfs
        :clover.search.dfs
        :clover.types
        :clover.resolution
        :clover.util
        )
  (:import-from :alexandria
                :median
                :variance)
  (:import-from :clover.unify
                :alphabet=)
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

(defmethod node-hash ((node clause-set))
  (sxhash (format nil "~A" node)))

(defmethod node-equality ((node1 clause-set) (node2 clause-set))
  (clause-set= node1 node2))


(defun %canonical-term-string (term var-index counter-cell out)
  "term を out へ直列化する。変数は var-index(変数シンボル->番号) に従い初出順で番号付け。
   constant は fterm のサブタイプのため fterm より先に分岐する。"
  (typecase term
    (vterm
     (let* ((v (vterm.var term))
            (idx (or (gethash v var-index)
                     (setf (gethash v var-index)
                           (prog1 (car counter-cell) (incf (car counter-cell)))))))
       (write-char #\? out)
       (princ idx out)))
    (constant
     (write-char #\# out)
     (write-string (symbol-name (constant.value term)) out))
    (fterm
     (write-string (symbol-name (fterm.fsymbol term)) out)
     (write-char #\( out)
     (loop :for a :in (fterm.args term)
           :for firstp := t :then nil
           :do (unless firstp (write-char #\, out))
               (%canonical-term-string a var-index counter-cell out))
     (write-char #\) out))
    (t (princ term out))))

(defun %canonical-clause-string (clause)
  "節を変数リネーム不変な文字列へ1パスで直列化する(rename等のオブジェクト再構築を伴わず軽量)。
   変数は節内の初出順に番号付け(節集合の alphabet= は節ごと独立の変数全単射のため節ローカルで正しい)。
   リテラルは clause.literals の順序のまま(リテラル内/順序や condensation までは畳まない)。"
  (let ((var-index (make-hash-table :test #'eq))
        (counter-cell (list 0))
        (out (make-string-output-stream)))
    (loop :for lit :in (clause.literals clause)
          :for firstp := t :then nil
          :do
          (unless firstp (write-char #\| out))
          (when (literal.negation lit) (write-char #\! out))
          (write-string (symbol-name (literal.predicate lit)) out)
          (write-char #\( out)
          (loop :for a :in (literal.args lit)
                :for f := t :then nil
                :do (unless f (write-char #\, out))
                    (%canonical-term-string a var-index counter-cell out))
          (write-char #\) out))
    (get-output-stream-string out)))

(defmethod node-canonical-key ((node clause-set))
  ;; α同値(厳密な変種)な clause-set を同一キー化する正準キー文字列。
  ;; 各節を変数初出順で1パス直列化し、節文字列をソートして連結(clause-set は節順非依存のため)。
  ;; これは alphabet= の過小近似(リテラル内順序や condensation は畳まない)であり、非等価な状態を
  ;; 誤って同一視しない=closed集合による枝刈りは健全。畳み損ねたぶんは重複排除が減るだけ。
  (format nil "~{~A~^/~}"
    (sort
      (mapcar #'%canonical-clause-string (clause-set.clauses node))
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

