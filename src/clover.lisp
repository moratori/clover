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
  (:import-from :clover.lib.util
                :measuring-time
                :make-deadline 
                :call-with-budget
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
  (:import-from :clover.converter
                :convert-to-equation-set
                )
  (:export
    :prove
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


(defmethod %prove-equation ((clause clause) (completed-rewrite-rule-set rewrite-rule-set))
  (loop
    :with status-accum := t
    :with supporting-info := nil
    :for lit :in (clause.literals clause)
    :do
    (multiple-value-bind (local-prove-status irreducible) 
        (start-trs lit completed-rewrite-rule-set)
      (push irreducible supporting-info)
      (setf status-accum (and local-prove-status status-accum)))
    :finally
    (return 
      (values 
        (if status-accum :proved :disproved)
        (nreverse supporting-info)))))

(defmethod %prove-clause-set ((clause-set clause-set))
  (multiple-value-bind (foundp node)
      (start-resolution clause-set)
    (values
      (if foundp :proved :unknown)
      node)))

(defmethod prove ((prove-input prove-input))
  (let* ((target 
         ;; 証明したい式そのもの。
         ;; 否定形にするかどうかは、あくまで証明のテクニックになにを用いるか(背理法(反駁)を使うかどうか)に依存する部分。
         ;; 呼び出し側は、素直に証明したい式そのものを入力する。
           (prove-input.target prove-input)) 
         (premises (prove-input.premises prove-input))
         (completed-rewrite-rule-set (prove-input.completed-rewrite-rule-set prove-input))
         (timeout-seconds (prove-input.timeout-seconds prove-input))
         (print-stream (prove-input.print-stream prove-input))

         (target-equation
           (cond
             ((typep target 'equation)
              (clause (list target)))
             ((equation-clause-p target)
              target)
             (t nil)))
         (premises-equation
           (typecase premises
             (equation-set premises)
             (clause-set (convert-to-equation-set premises))
             (t nil)))
         (deadline       (make-deadline timeout-seconds))
         (completion-cap (and timeout-seconds (* timeout-seconds 0.85)))
         (engine         :none))   ; 戦略選択時に setf。timeout 時もこの値が使われる
    (multiple-value-bind (elapsed status supporting-info result-rrs)
        (measuring-time
          (handler-case

              ;; 等式については、なるべく導出に落とさないように先にcondの条件を記載していることに注意
              (cond
                ;; 行1: rrs あり + 等式ターゲット → 書き換えに残り時間を全部使う
                ((and target-equation completed-rewrite-rule-set)
                 (setf engine :rewriting)
                 (multiple-value-bind (st info)
                     (call-with-budget deadline nil
                       (lambda ()
                         (%prove-equation target-equation completed-rewrite-rule-set)))
                   (values st info completed-rewrite-rule-set)))

                ;; 行2-3: 完備化(≦ min(残り, 85%)) → 書き換え(残り全部)
                ((and target-equation premises-equation)
                 (setf engine :rewriting)
                 (multiple-value-bind (flag ordering completed)
                     (call-with-budget deadline completion-cap
                       (lambda ()
                         (multi-kb-completion premises-equation
                                              *completion-giveup-threshold*)))
                   (declare (ignore ordering))
                   (if (not flag)
                       (values :unknown (list :completion-failed) nil)
                       (multiple-value-bind (st info)
                           (call-with-budget deadline nil   ; ← 残余時間が自動で上限になる
                             (lambda ()
                               (%prove-equation target-equation completed)))
                         (values st info completed)))))

                ((and (typep target 'clause) (or (typep premises 'clause-set) (null premises)))
                 (setf engine :resolution)
                 (let* ((negation
                          (clause 
                            (mapcar
                              (lambda (lit)
                                ;; litが等式である可能性は一応あることに留意
                                (literal
                                  (not (literal.negation lit))
                                  (literal.predicate lit)
                                  (literal.args lit)))
                              (clause.literals target))
                            nil nil nil :conseq))
                        (clause-set
                          (if (null premises)
                              (clause-set (list negation))
                              (clause-set
                                (cons negation (clause-set.clauses premises))))))
                   (multiple-value-bind (st info)
                       (call-with-budget 
                         deadline nil
                         (lambda ()
                           (%prove-clause-set clause-set)))
                         (values st info nil))))

                (t (values :unknown (list :not-implemented) nil)))

            (sb-ext:timeout ()
              (values :timeout nil nil))))
      (prove-result status engine elapsed supporting-info result-rrs))))

