(defpackage clover.completion
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.rename
        :clover.rewrite
        :clover.termorder
        )
  (:export
    :kb-completion
    )
  )
(in-package :clover.completion)


(defmethod delete-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (values 
    (equation-set
      (remove-duplicates
        (remove-if
          #'tautology-equation-p
          (equation-set.equations equation-set))
        :test #'equation=))
    rewrite-rule-set))

(defmethod compose-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  )

(defmethod deduce-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  )

(defmethod simplify-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  )

(defmethod orient-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  )


(defmethod collapse-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  )





(defmethod apply-inference-rules ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (let ((ret
          (reduce
            (lambda (r f)
              (multiple-value-bind (eqs rrls)
                  (funcall f (car r) (cdr r))
                (cons eqs rrls)))
            (list 
                  #'simplify-rule
                  #'delete-rule  
                  #'compose-rule
                  #'collapse-rule
                  #'deduce-rule
                  #'orient-rule)
            :initial-value (cons equation-set rewrite-rule-set))))
    (values (car ret) (cdr ret))))

(defmethod kb-completion ((equation-set equation-set) giveup-threshold)
  (let ((result-equation-set equation-set)
        (result-rewrite-rule-set (rewrite-rule-set nil))
        (cnt 0))

    (catch 'kb-completion_failed
           (loop
             :while (and (not (null (equation-set.equations result-equation-set)))
                         (> giveup-threshold cnt))
             :do
             (incf cnt)
             (multiple-value-bind (eqs rrls)
                 (apply-inference-rules
                   result-equation-set
                   result-rewrite-rule-set)
               (setf result-equation-set eqs
                     result-rewrite-rule-set rrls))))

    (when (null (equation-set.equations result-equation-set))
      result-rewrite-rule-set)))

