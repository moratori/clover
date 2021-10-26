(defpackage clover.termorder
  (:use :cl
        :clover.conditions
        :clover.types
        :clover.util)
  (:export
    :function-symbol-order
    :term<=
    :term<
    ))
(in-package :clover.termorder)


(defmethod function-symbol-order ((ordering function-symbol-ordering)
                                  (left symbol)
                                  (right symbol)
                                  (algorithm (eql :lpo)))
  (let* ((ordering (function-symbol-ordering.ordering ordering))
         (pos-left (position left ordering :test #'eq))
         (pos-right (position right ordering :test #'eq)))
    (unless (and ordering pos-left pos-right)
      (error "unable to get position ~A,~A in ~A" left right ordering))
    (< pos-left pos-right)))


(defun lexicographic-order< (args1 args2 ordering)
  (when (and (not (null args1))
             (not (null args2)))
    (let ((head1 (car args1))
          (head2 (car args2)))
      (if (term< head1 head2 ordering :lpo)
        (= (length args1) (length args2))
        (and (term= head1 head2)
             (lexicographic-order< (cdr args1) (cdr args2) ordering))))))




(defmethod term<= ((term1 term) (term2 term) (ordering function-symbol-ordering) (algorithm (eql :lpo)))
  (or
    (term= term1 term2)
    (term< term1 term2 ordering algorithm)))


(defmethod term< ((term1 vterm) (term2 vterm) (ordering function-symbol-ordering) (algorithm (eql :lpo)))
  nil)

(defmethod term< ((term1 vterm) (term2 fterm) (ordering function-symbol-ordering) (algorithm (eql :lpo)))
  (some
    (lambda (var)
      (term= term1 var))
    (collect-variables term2)))

(defmethod term< ((term1 fterm) (term2 vterm) (ordering function-symbol-ordering)  (algorithm (eql :lpo)))
  nil)

(defmethod term< ((term1 fterm) (term2 fterm) (ordering function-symbol-ordering) (algorithm (eql :lpo)))
  (let ((fsymbol1 (fterm.fsymbol term1))
        (fsymbol2 (fterm.fsymbol term2))
        (args1 (fterm.args term1))
        (args2 (fterm.args term2)))
    (or
      (some
        (lambda (arg)
          (term<= term1 arg ordering algorithm))
        (fterm.args term2))
      (and
        (every
          (lambda (arg)
            (term< arg term2 ordering algorithm))
          args1)
        (or
          (function-symbol-order ordering fsymbol1 fsymbol2 algorithm)
          (and
            (eq fsymbol1 fsymbol2)
            (lexicographic-order< args1 args2 ordering)))))))
