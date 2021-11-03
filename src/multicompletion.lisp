(defpackage clover.multicompletion
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types
        :clover.completion
        )
  (:export
    :multi-kb-completion
    )
  )
(in-package :clover.multicompletion)


(defun initialize-lparallel-kernel ()
  (unless lparallel:*kernel*
    (let ((cpu-number
            (handler-case
                (cpus:get-number-of-processors)
              (error (c) 1))))
      (setf lparallel:*kernel* 
            (lparallel:make-kernel
              (if (>= 1 cpu-number) 1 (1- cpu-number)))))))


(defun permutation (candidate)
  (cond
    ((null candidate) nil)
    ((= (length candidate) 1) (list candidate))
    ((< 1 (length candidate))
     (loop
       :for elm :in candidate
       :for idx :from 0
       :for left := (subseq candidate 0 idx)
       :for right := (subseq candidate (1+ idx))
       :for other := (append left right)
       :append
       (mapcar
         (lambda (x)
           (cons elm x))
         (permutation other))))))


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


(defmethod make-ordering ((equation-set equation-set))
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
    (mapcar
      (lambda (perm)
        (function-symbol-ordering
          (append constant-symbols perm)))
      function-symbols-permutation)))

(defmethod multi-kb-completion ((equation-set equation-set) giveup-threshold)
  (initialize-lparallel-kernel)
  (lparallel.cognate:psome
    (lambda (ordering)
      (handler-case
          (kb-completion
            equation-set
            ordering
            giveup-threshold)
        (clover-toplevel-condition (c) nil)
        (condition (c)
          (format *standard-output*
                  "~%unexpected condition ~A occurred while completion" c)
          nil)))
    (make-ordering equation-set)))


