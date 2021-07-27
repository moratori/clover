(defpackage clover.termorder
  (:use :cl
        :clover.conditions
        :clover.types
        :clover.util)
  (:export
    :term<
    ))
(in-package :clover.termorder)


(defmethod symbol-order ((sym1 symbol) (sym2 symbol))
  (and (string< (string-upcase (symbol-name sym1))
                (string-upcase (symbol-name sym2)))
       t))

(defmethod symbol-order ((fterm1 fterm) (fterm2 fterm))
  (let ((arity1 (length (fterm.args fterm1)))
        (arity2 (length (fterm.args fterm2))))
    (cond
      ((= arity1 arity2) 
       (symbol-order (fterm.fsymbol fterm1)
                     (fterm.fsymbol fterm2)))
      ((zerop arity1) t)
      (t
       (< arity1 arity2)))))

(defmethod %count-fterm-application ((term vterm))
  0)

(defmethod %count-fterm-application ((term constant))
  0)

(defmethod %count-fterm-application ((term fterm))
  (1+ 
   (reduce (lambda (r x)
             (+ r (%count-fterm-application x)))
           (fterm.args term)
           :initial-value 0)))


(defmethod term< ((term1 vterm) (term2 vterm) (algorithm (eql :original)))
  (symbol-order (vterm.var term1) (vterm.var term2)))

(defmethod term< ((term1 vterm) (term2 constant) (algorithm (eql :original)))
  nil)

(defmethod term< ((term1 vterm) (term2 fterm) (algorithm (eql :original)))
  ;; term2 が groundであった場合は、term1の方が大きい？
  t)

(defmethod term< ((term1 constant) (term2 vterm) (algorithm (eql :original)))
  t)

(defmethod term< ((term1 fterm) (term2 vterm) (algorithm (eql :original)))
  ;; term1 が groundであった場合は、term1の方が小さい？
  nil)

(defmethod term< ((term1 constant) (term2 constant) (algorithm (eql :original)))
  (symbol-order
    (constant.value term1)
    (constant.value term2)))

(defmethod term< ((term1 constant) (term2 fterm) (algorithm (eql :original)))
  t)

(defmethod term< ((term1 fterm) (term2 constant) (algorithm (eql :original)))
  nil)

(defmethod term< ((term1 fterm) (term2 fterm) (algorithm (eql :original)))
  (cond
    ((and (not (ground-term-p term1))
            (ground-term-p term2))
     nil)
    ((and (ground-term-p term1)
          (not (ground-term-p term2)))
     t)
    ;; term1 = g(t1, t2, ,,, tn)
    ;; term2 = f(s1, s2, ,,, sm)
    ;; exist i: si > term1 or s1 = term1
    ((some
       (lambda (x)
         (or 
           (term< term1 x algorithm)
           (term= term1 x)))
       (fterm.args term2))
     t)
    (t
     ;; term1 = term2 = ground
     ;;       or
     ;; term1 = term2 = func
     (let ((left-complexity  (%count-fterm-application term1))
           (right-complexity (%count-fterm-application term2)))
       (cond
         ((= left-complexity right-complexity)
          (and (string< (format nil "~A" term1)
                        (format nil "~A" term2))
               t))
         (t (< left-complexity right-complexity)))))))






(defun lexicographic-order< (args1 args2)
  (loop
    :named exit
    :for arg1 :in args1
    :for arg2 :in args2
    :for index :from 0
    :do
    (when (and (term< arg1 arg2 :lpo)
               (every #'term=
                      (subseq args1 0 index)
                      (subseq args2 0 index)))
      (return-from exit t))))

(defmethod term< ((term1 vterm) (term2 vterm) (algorithm (eql :lpo)))
  nil)

(defmethod term< ((term1 fterm) (term2 vterm) (algorithm (eql :lpo)))
  nil)

(defmethod term< ((term1 vterm) (term2 fterm) (algorithm (eql :lpo)))
  (some
    (lambda (arg)
      (or
        (term< term1 arg algorithm)
        (term= term1 arg)))
    (fterm.args term2)))

(defmethod term< ((term1 fterm) (term2 fterm) (algorithm (eql :lpo)))
  (let ((fsymbol1 (fterm.fsymbol term1))
        (fsymbol2 (fterm.fsymbol term2))
        (args1 (fterm.args term1))
        (args2 (fterm.args term2)))
    (or
      (some
        (lambda (arg)
          (or
            (term< term1 arg algorithm)
            (term= term1 arg)))
        (fterm.args term2))
      (and
        (every
          (lambda (arg)
            (term< arg term2 algorithm))
          args1)
        (or
          (symbol-order term1 term2)
          (and
            (eq fsymbol1 fsymbol2)
            (lexicographic-order< args1 args2)))))))

