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
  (:import-from :clover.criticalpair
                :all-critical-pair)
  (:export
    :kb-completion
    )
  )
(in-package :clover.completion)

(defparameter *debug-print* nil)


(defmethod %orient ((equation equation))
  (let ((left (equation.left equation))
        (right (equation.right equation)))
    (cond
      ((term< left right *term-order-algorithm*)
       (rename (rewrite-rule right left)))
      ((term< right left *term-order-algorithm*)
       (rename (rewrite-rule left right)))
      (t (error "unable to orient equation ~A by ~A"
                equation
                *term-order-algorithm*)))))

(defmethod %collect-small-rules ((rule rewrite-rule) (rules rewrite-rule-set))
  "x <- R のうち、ruleによってx.leftを書き換えることのできないrを集める"
  (rewrite-rule-set
    (remove-if
      (lambda (x)
        (let* ((src (rewrite-rule.src x))
               (dst (rewrite-rule.dst x))
               (rewrote (rewrite src rule)))
          (not (term= rewrote src))))
      (rewrite-rule-set.rewrite-rules rules)))) 

(defmethod delete-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (values 
    (equation-set
      (remove-duplicates
        (remove-if
          #'tautology-p
          (equation-set.equations equation-set))
        :test (lambda (x y)
                (or (equation= x y)
                    (alphabet= x y)))))
    (rewrite-rule-set
      (remove-duplicates
        (remove-if
          #'tautology-p
          (rewrite-rule-set.rewrite-rules rewrite-rule-set))
        :test (lambda (x y)
                (or (rewrite-rule= x y)
                    (alphabet= x y)))))))

(defmethod simplify-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (values
    (equation-set
      (mapcar
        (lambda (x) 
          (rewrite-final x rewrite-rule-set))
        (equation-set.equations equation-set)))
    rewrite-rule-set))



(defmethod infer :around (rule equation-set rewrite-rule-set)
  (multiple-value-bind (applied-equation-set applied-rewrite-rule-set)
      (call-next-method rule equation-set rewrite-rule-set)
    (multiple-value-bind (simplified-e simplified-r)
        (simplify-rule applied-equation-set applied-rewrite-rule-set)
      (multiple-value-bind (deleted-e deleted-r)
          (delete-rule simplified-e simplified-r)
        (values deleted-e deleted-r)))))

(defmethod infer ((rule (eql :compose))
                  (equation-set equation-set)
                  (rewrite-rule-set rewrite-rule-set))
  (values
    equation-set
    (rewrite-rule-set
      (mapcar
        (lambda (x)
          (let ((src (rewrite-rule.src x))
                (dst (rewrite-rule.dst x)))
            (rewrite-rule
              src
              (rewrite-final dst rewrite-rule-set))))
        (rewrite-rule-set.rewrite-rules rewrite-rule-set)))))

(defmethod infer ((rule (eql :deduce))
                  (equation-set equation-set)
                  (rewrite-rule-set rewrite-rule-set))
  (let ((eqs (all-critical-pair rewrite-rule-set)))
    (values
      (equation-set
        (append
          (equation-set.equations equation-set)
          (equation-set.equations eqs)))
      rewrite-rule-set)))


(defmethod infer ((rule (eql :orient))
                  (equation-set equation-set)
                  (rewrite-rule-set rewrite-rule-set))
  (let (right< left<)
    (loop
      :for equation :in (equation-set.equations equation-set)
      :for left  := (equation.left equation)
      :for right := (equation.right equation)
      :if (term< left right *term-order-algorithm*) :do (push equation right<)
      :if (term< right left *term-order-algorithm*) :do (push equation left<))
    (when (and (null right<) (null left<))
      (throw 'kb-completion_failed nil))
    (let* ((candidate
             (append right< left<))
           (new-rules
             (mapcar #'%orient candidate)))
      (values
        (equation-set
          (remove-if
            (lambda (x)
              (member x candidate :test #'equation=))
            (equation-set.equations equation-set)))
        (rewrite-rule-set
          (append
            new-rules
            (rewrite-rule-set.rewrite-rules rewrite-rule-set)))))))


(defmethod infer ((rule (eql :collapse))
                  (equation-set equation-set)
                  (rewrite-rule-set rewrite-rule-set))
  (let* ((rules
           (rewrite-rule-set.rewrite-rules rewrite-rule-set))
         (smaller-rules
           (mapcar
             (lambda (rule)
               (%collect-small-rules rule rewrite-rule-set))
             rules))
         (applied-small-rules
           (mapcar
             (lambda (target-rule small)
               (rewrite-final
                 (rewrite-rule.src target-rule)
                 small))
             rules
             smaller-rules))
         (changed
           (loop
             :for rule :in rules
             :for applied-src :in applied-small-rules
             :for original-src := (rewrite-rule.src rule)
             :if (not (term= original-src applied-src))
             :collect (cons rule applied-src))))
    (if (null changed)
        (values equation-set rewrite-rule-set)
        (values
          (equation-set
            (append
              (mapcar
                (lambda (x)
                  (equation
                    nil
                    (rewrite-rule.dst (car x))
                    (cdr x)))
                changed)
              (equation-set.equations equation-set)))
          (rewrite-rule-set
            (remove-if
              (lambda (x)
                (find-if
                  (lambda (y) 
                    (rewrite-rule= x (car y)))
                  changed))
              rules))))))


(defmethod apply-inference-rules ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (let ((ret
          (reduce
            (lambda (target rule)
              (multiple-value-bind (eqs rrls)
                  (infer rule (car target) (cdr target))

                (when *debug-print*
                  (format t "~%@@@@@ infered by ~A and simplify/delete @@@@@~%" rule)
                  (format t "      ret equation-set     = ~A~%"
                          (rename-for-human-readable-printing eqs))
                  (format t "      ret rewrite-rule-set = ~A~%"
                          (rename-for-human-readable-printing rrls))
                  (format t "      added equation       = ~A~%"
                          (set-difference (equation-set.equations
                                            eqs) 
                                          (equation-set.equations
                                            (car target))
                                          :test #'equation=))
                  (format t "      removed equation     = ~A~%"
                          (set-difference (equation-set.equations
                                            (car target))
                                          (equation-set.equations
                                            eqs)
                                          :test #'equation=))
                  (format t "      added rwrule         = ~A~%"
                          (set-difference (rewrite-rule-set.rewrite-rules
                                            rrls)
                                          (rewrite-rule-set.rewrite-rules
                                            (cdr target))
                                          :test #'rewrite-rule=))
                  (format t "      removed rwrule       = ~A~%"
                          (set-difference (rewrite-rule-set.rewrite-rules
                                            (cdr target))
                                          (rewrite-rule-set.rewrite-rules
                                            rrls)
                                          :test #'rewrite-rule=)))

                (cons eqs rrls)))
            (list :collapse
                  :compose
                  :orient 
                  :deduce)
            :initial-value (cons equation-set rewrite-rule-set))))
    (values (car ret) (cdr ret))))

(defmethod kb-completion ((equation-set equation-set) giveup-threshold)
  (let ((result-equation-set
          (rename equation-set))
        (result-rewrite-rule-set
          (rewrite-rule-set nil))
        (cnt 0))

    (catch 'kb-completion_failed
           (loop
             :while (and (not (null (equation-set.equations result-equation-set)))
                         (> giveup-threshold cnt))
             :do

             (when *debug-print*
               (format t "~%######################################################################~%")
               (format t "# Completion ROUND ~A~%" cnt)
               (format t "######################################################################~%")
               (format t "initial equation-set = ~%    ~A~%"
                       (rename-for-human-readable-printing result-equation-set))
               (format t "initial rewrite-rule-set = ~%    ~A~%"
                       (rename-for-human-readable-printing result-rewrite-rule-set))
               (force-output *standard-output*)
               (sleep 0.1))

             (incf cnt)
             (multiple-value-bind (eqs rrls)
                 (apply-inference-rules
                   result-equation-set
                   result-rewrite-rule-set)
               (setf result-equation-set eqs
                     result-rewrite-rule-set rrls))))

    (when (null (equation-set.equations result-equation-set))
      result-rewrite-rule-set)))

