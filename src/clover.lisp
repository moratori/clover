(defpackage clover.clover
  (:use :cl
        :clover.parameters
        :clover.lib.search.common
        :clover.lib.search.astar
        :clover.lib.search.iddfs
        :clover.lib.search.dfs
        :clover.types
        :clover.resolution
        :clover.logical-predicates
        :clover.heuristics
        )
  (:import-from :clover.equality
                :term=
                :term/=
                :clause=)
  (:import-from :clover.canonicalization
                :canonical-clause-string)
  (:import-from :clover.rewrite
                :rewrite-final)
  (:import-from :clover.multicompletion
                :multi-kb-completion)
  (:export
    :start-resolution
    :start-trs
    :start-completion
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




(defmethod start-completion ((equation-set equation-set) giveup-threshold)
  (let ((*error-output* (make-two-way-stream
                          (make-concatenated-stream)
                          (make-broadcast-stream))))
    (handler-case
        (multi-kb-completion equation-set giveup-threshold)
      (condition (c)
        (values nil nil nil))))) 


(defmethod start-trs ((expr equation) (rewrite-rule-set rewrite-rule-set))
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


(defmethod start-resolution ((clause-set clause-set))
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

