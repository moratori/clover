(defpackage clover.termorder
  (:use :cl
        :clover.conditions
        :clover.types
        :clover.util)
  (:export
    :term<
    ))
(in-package :clover.termorder)


(defun %symbol-order (sym1 sym2)
  (and (string< (string-upcase (symbol-name sym1))
                (string-upcase (symbol-name sym2)))
       t))

(defun %fsymbol-order-for-dictionary (fterm1 fterm2)
  (let ((arity1 (length (fterm.args fterm1)))
        (arity2 (length (fterm.args fterm2))))
    (cond
      ((< arity1 arity2) t)
      ((= arity1 arity2) 
       (%symbol-order (fterm.fsymbol fterm1) (fterm.fsymbol fterm2)))
      (t nil))))

(defmethod %count-fterm-application ((term vterm))
  0)

(defmethod %count-fterm-application ((term fterm))
  (if (constant-p term)
      0
      (1+ (loop :for arg :in (fterm.args term)
                :sum (%count-fterm-application arg)))))



(defmethod term< ((term1 vterm) (term2 vterm) (algorithm (eql :dictionary)))
  nil)

(defmethod term< ((term1 vterm) (term2 fterm) (algorithm (eql :dictionary)))
  t)

(defmethod term< ((term1 fterm) (term2 vterm) (algorithm (eql :dictionary)))
  nil)

(defmethod term< ((term1 fterm) (term2 fterm) (algorithm (eql :dictionary)))
  (let ((args1 (fterm.args term1))
        (args2 (fterm.args term2)))
    (or 
      (some
        (lambda (x)
          (or (term< term1 x algorithm) 
              (term= term1 x)))
        args2)
      (and
        (every
          (lambda (x)
            (term< x term2 algorithm))
          args1)
        (or 
          (%fsymbol-order-for-dictionary term1 term2)
          (and
            (string= (string-upcase (symbol-name (fterm.fsymbol term1)))
                     (string-upcase (symbol-name (fterm.fsymbol term2))))
            (= (length args1) (length args2))
            ;; term1 = f(t1, t2, t3, ... , tn)
            ;; term2 = f(s1, s2, s3, ... , sm)
            ;; exist i : (si > ti & forall k < i : tk = sk)
            (let ((first-index 
                    (loop
                      :named exit
                      :for tn :in args1
                      :for sm :in args2
                      :for index :from 0
                      :if (term< tn sm algorithm)
                      :do (return-from exit index))))
              (every
                #'term=
                (subseq args1 0 first-index)
                (subseq args2 0 first-index)))))))))




(defmethod term< ((term1 vterm) (term2 vterm) (algorithm (eql :original)))
  (%symbol-order (vterm.var term1) (vterm.var term2)))

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
  (%symbol-order
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

