(defpackage clover.multicompletion
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types
        :clover.completion
        )
  (:import-from :generators
                :make-generator
                :yield
                :next
                :stop-iteration)
  (:export
    :multi-kb-completion
    :toplevel-completion
    ))
(in-package :clover.multicompletion)


(let (cached)
  (defun get-number-of-processors ()
    (if cached cached
        (let ((num
                (handler-case
                    (cpus:get-number-of-processors)
                  (error (c) 1))))
          (setf cached num)))))


(defun initialize-lparallel-kernel ()
  (when lparallel:*kernel*
    (lparallel:end-kernel :wait nil))
  (unless lparallel:*kernel*
    (let ((cpu-number
            (get-number-of-processors)))
      (setf lparallel:*kernel* 
            (lparallel:make-kernel
              (if (>= 1 cpu-number) 1 (1- cpu-number)))))))

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


(defun permutation (elements)
  (make-generator ()
    (if (<= (length elements) 1)
        (yield elements)
        (handler-case
            (loop
              :with gen := (permutation (subseq elements 1))
              :for perm := (next gen)
              :do
              (loop
                :for i :from 0 :below (length elements)
                :do
                (yield 
                  (append 
                    (subseq perm 0 i)
                    (subseq elements 0 1)
                    (subseq perm i)))))
          (stop-iteration (c) 
            (declare (ignore c)) nil)))))

(defun take (n gen)
  (let (result)
    (handler-case 
        (dotimes (i n)
          (let ((value (next gen)))
            (when value
              (push value result))))
      (stop-iteration (c)
        (declare (ignore c))))
    result))



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
                    (lparallel.cognate:psome
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

(defmethod toplevel-completion ((equation-set equation-set) giveup-threshold)
  (let ((*error-output* (make-two-way-stream
                          (make-concatenated-stream)
                          (make-broadcast-stream))))
    (handler-case
        (multi-kb-completion equation-set giveup-threshold)
      (condition (c)
        (values nil nil nil)))))

