(defpackage clover.multicompletion
  (:use :cl
        :clover.parameters
        :clover.conditions
        :clover.types
        :clover.completion
        )
  (:import-from :clover.lib.parallel
                :initialize-lparallel-kernel
                :psome/kill)
  (:import-from :clover.lib.util
                :permutation
                :take)
  (:export
    :multi-kb-completion
    ))
(in-package :clover.multicompletion)



(defmethod collect-constant-symbol ((term term))
  nil)

(defmethod collect-constant-symbol ((term constant))
  (list (constant.value term)))

(defmethod collect-constant-symbol ((term fterm))
  (remove-duplicates
    (mapcan
      #'collect-constant-symbol
      (fterm.args term))
    :test #'eq))


(defmethod collect-function-symbol ((term term))
  nil)

(defmethod collect-function-symbol ((term constant))
  nil)

(defmethod collect-function-symbol ((term fterm))
  (remove-duplicates
    (cons (fterm.fsymbol term)
          (mapcan #'collect-function-symbol (fterm.args term)))
    :test #'eq))


(defmethod collect-symbol ((equation-set equation-set))
  (let* ((eqs
           (equation-set.equations equation-set))
         (all-terms
           (mapcan
             (lambda (x)
               (list (equation.left x) (equation.right x)))
             eqs))
         (constant-symbols
           (sort
             (remove-duplicates
               (mapcan #'collect-constant-symbol all-terms)
               :test #'eq)
             (lambda (x y)
               (and (string< (symbol-name x) (symbol-name y)) t))))
         (function-symbols
           (remove-duplicates
             (mapcan #'collect-function-symbol all-terms)
             :test #'eq))
         (function-symbols-permutation
           (permutation function-symbols)))
     (values constant-symbols function-symbols)))


(defmethod multi-kb-completion ((equation-set equation-set) giveup-threshold)
  (initialize-lparallel-kernel)
  (multiple-value-bind (constant-symbols function-symbols)
      (collect-symbol equation-set)
    (if (null function-symbols)
        (kb-completion
          equation-set
          (function-symbol-ordering constant-symbols)
          giveup-threshold)
        (let ((fun-sym-order-generator
                (permutation function-symbols))
              result)
          (loop
            :named exit
            :for function-order := (take *take-limit-from-permutation-generator*
                                         fun-sym-order-generator)
            :while (not (null function-order))
            :for actual-order := (mapcar 
                                   (lambda (order)
                                     (function-symbol-ordering
                                       (append constant-symbols order)))
                                   function-order)
            :do
            (let ((local-result
                    (psome/kill
                      (lambda (order)
                        (handler-case
                            (multiple-value-bind (flag ordering rrs)
                                (kb-completion equation-set order giveup-threshold)
                              (when flag
                                (list flag ordering rrs)))
                          (clover-toplevel-condition (c) nil)
                          (condition (c)
                            (format *standard-output*
                                    "~%unexpected condition ~A occurred while single completion thread" c)
                            nil)))
                      actual-order)))
              (when local-result
                (setf result local-result)
                (return-from exit))))
          (if result
              (values-list result)
              (values nil nil nil))))))


