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

(defparameter *debug-print* 0)


(defmethod delete-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (values 
    (equation-set
      (remove-duplicates
        (remove-if
          #'tautology-equation-p
          (equation-set.equations equation-set))
        :test (lambda (x y)
                (or (equation= x y)
                    (alphabet= x y)))))
    (rewrite-rule-set
      (remove-duplicates
        (rewrite-rule-set.rewrite-rules rewrite-rule-set)
        :test (lambda (x y)
                (or (rewrite-rule= x y)
                    (alphabet= x y)))))))

(defmethod simplify-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (values
    (equation-set
      (mapcar
        #'rewrite-final
        (equation-set.equations equation-set)))
    rewrite-rule-set))

(defmethod compose-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
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

(defmethod deduce-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (let ((eqs (all-critical-pair rewrite-rule-set)))
    (values
      (equation-set
        (append
          (equation-set.equations equation-set)
          (equation-set.equations eqs)))
      rewrite-rule-set)))

(defmethod orient-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (let (right< left<)
    (loop
      :for equation :in (equation-set.equations equation-set)
      :for left  := (equation.left equation)
      :for right := (equation.right equation)
      :if (term< left right *term-order-algorithm*) :do (push equation right<)
      :if (term< right left *term-order-algorithm*) :do (push equation left<))
    (when (and (null right<) (null left<))
      (throw 'kb-completion_failed nil))
    (values
      (equation-set
        (remove-if
          (lambda (x)
            (or
              (member x right< :test #'equation=)
              (member x left< :test #'equation=)))
          (equation-set.equations equation-set)))
      (rewrite-rule-set
        (append
          (rewrite-rule-set.rewrite-rules rewrite-rule-set)
          (mapcar
            (lambda (x)
              (rename (rewrite-rule 
                        (equation.right x)
                        (equation.left x))))
            right<)
          (mapcar
            (lambda (x)
              (rename (rewrite-rule 
                        (equation.left x)
                        (equation.right x))))
            left<))))))

(defmethod collapse-rule ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (let* ((rules
           (rewrite-rule-set.rewrite-rules rewrite-rule-set))
         (rewroted-by-smaller-rules
           (mapcar
             (lambda (r1)
               (let ((smaller
                       (rewrite-rule-set
                         (remove-if-not
                           (lambda (r2)
                             (rewrite-rule< r2 r1))
                           rules)))
                     (src (rewrite-rule.src r1))
                     (dst (rewrite-rule.dst r1)))
                 (list r1 (rewrite-final src smaller) dst)))
            rules)))
    (values
      (equation-set
        (append
          (equation-set.equations equation-set)
          (mapcar
            (lambda (x)
              (destructuring-bind (rule rewroted-src original-dst) x
                (equation
                  nil
                  original-dst
                  rewroted-src)))
            (remove-if
              (lambda (x)
                (destructuring-bind (rule rewroted-src original-dst) x
                  (let ((src (rewrite-rule.src rule))
                        (dst (rewrite-rule.dst rule)))
                    (and 
                      (term= src rewroted-src)))))
              rewroted-by-smaller-rules))))
      (rewrite-rule-set
        (remove-if
          (lambda (x)
            (find-if
              (lambda (y)
                (destructuring-bind (rule rewroted-src original-dst) y
                  (let ((src (rewrite-rule.src x))
                        (dst (rewrite-rule.dst x)))
                    (and
                      (rewrite-rule= rule x)
                      (not (term= src rewroted-src))))))
              rewroted-by-smaller-rules))
          rules)))))




(defmethod apply-inference-rules ((equation-set equation-set) (rewrite-rule-set rewrite-rule-set))
  (let ((ret
          (reduce
            (lambda (r f)
              (multiple-value-bind (eqs rrls)
                  (funcall f (car r) (cdr r))
                (multiple-value-bind (eqs1 rrls1)
                    (simplify-rule eqs rrls)
                  (multiple-value-bind (eqs2 rrls2)
                      (delete-rule eqs1 rrls1)
                    (cons eqs2 rrls2)))))
            (list 
                  #'compose-rule
                  #'orient-rule
                  #'collapse-rule
                  #'deduce-rule)
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
             (when (and (typep *debug-print* 'number)
                        (< 0 *debug-print*))
               (format t "~%#################### Completion ROUND ~A~%" cnt)
               (format t "equation-set = ~%    ~A~%"
                       (rename-for-human-readable-printing result-equation-set))
               (format t "rewrite-rule-set = ~%    ~A~%"
                       (rename-for-human-readable-printing result-rewrite-rule-set))
               (force-output *standard-output*)
               (sleep *debug-print*))
             (incf cnt)
             (multiple-value-bind (eqs rrls)
                 (apply-inference-rules
                   result-equation-set
                   result-rewrite-rule-set)
               (setf result-equation-set eqs
                     result-rewrite-rule-set rrls))))

    (when (null (equation-set.equations result-equation-set))
      result-rewrite-rule-set)))

