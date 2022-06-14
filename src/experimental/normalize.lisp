(defpackage clover.experimental.normalize
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types
        :clover.util
        ))
(in-package :clover.experimental.normalize)

(defgeneric normalize-equation-set (equation-set)
  (:documentation
   "式集合Eを正規化する
    定数記号は、C0, C1, C2, ... の形式とする。
    関数記号は、f0, f1, f2, ... の形式とする。
    変数記号は、x0, x1, x2, ... の形式とする。
    例：
    {plus(ZERO,x) = x,
     plus(plus(x,y),z) = plus(x, plus(y,z)),
     plus(i(x),x) = ZERO}
       |
       |
      \|/
    {f0(C0,x0) = x0,
     f0(f0(x0,x1),x2) = f0(x0, f0(x1, x2)),
     f0(f1(x0),x0) = C0}"))

#|
##### same schema
1
hoge(hoge(x, y), fuga(x)) = piyo(y, x)
piyo(x, y) = neko(y, x)

2
h(h(x, y), f(x)) = p(y, x)
p(x, y) = n(y, x)

3
f(f(x, y), g(x)) = h(y, x)
h(x, y) = j(y, x)

##### not same schema to 1~3
4
h(h(x, y), f(x)) = p(y, x)
h(x, y) = n(y, x)

5
h(h(x, y), f(x)) = p(y, x)
k(x, y) = n(y, x)

6
h(x, y) = p(y, x)
k(x, y) = n(y, x)

7
hoge(hoge(x, x), fuga(x)) = piyo(y, x)
piyo(x, y) = neko(y, x)

8
hoge(hoge(x, y), fuga(x)) = piyo(y, x)
piyo(x, y) = neko(y, A)

##### procedure

1. get structural matching and get fsymbol binding
2. check null binding if exist, then fail
2. check consistency of fsymbol binding
3. apply binding to trusted equation set
4. check mgu

##### memo

A: Equation-set1
   1: term1-left = term1-right
   2: term2-left = term2-right
   .
   .
   n: termn-left = termn-right

B: Equation-set2
   1: term1-left = term1-right
   2: term2-left = term2-right
   .
   .
   m: termm-left = termm-right

------------------------------

A1: (binding1 binding2 ... )
A2: (binding1 binding2 ... )
.
.
An: (binding1 binding2 ... )


|#


(defgeneric structual-matching (term1 term2)
  (:documentation "変数名や関数名を無視し、項の構造が一致しているかチェックする"))

(defmethod structual-matching ((term1 vterm) (term2 vterm))
  t)

(defmethod structual-matching ((term1 constant) (term2 constant))
  t)

(defmethod structual-matching ((term1 t) (term2 t))
  nil)

(defmethod structual-matching ((term1 fterm) (term2 fterm))
  (let ((args1 (fterm.args term1))
        (args2 (fterm.args term2)))
    (and
      (= (length args1) (length args2))
      (every #'structual-matching args1 args2))))



(defmethod %get-function-symbol-binding ((term1 vterm) (term2 vterm))
  nil)

(defmethod %get-function-symbol-binding ((term1 constant) (term2 constant))
  (list (cons (string-upcase (string (constant.value term1)))
              (string-upcase (string (constant.value term2))))))

(defmethod %get-function-symbol-binding ((term1 fterm) (term2 fterm))
  (cons
    (cons (string-downcase (string (fterm.fsymbol term1)))
          (string-downcase (string (fterm.fsymbol term2))))
    (mapcan
      #'get-function-symbol-binding
      (fterm.args term1)
      (fterm.args term2))))

(defun get-function-symbol-binding (term1 term2)
  (unless (structual-matching term1 term2)
    (error "unmatched structure"))
  (let* ((tmp
           (%get-function-symbol-binding term1 term2))
         (result
           (remove-duplicates
             tmp :test
             (lambda (x y)
               (and
                 (string= (car x) (car y))
                 (string= (cdr x) (cdr y)))))))
    (unless (consistent result)
      (error "unconsistent result ~A~%" result))
    result))




(defun consistent-binding (binding)
#|
  ((f . g) (f . h)) -> x

|#
  )


(defmethod normalize-equation-set ((equation-set equation-set))
  )

(defmethod include-same-schema ((equation-set1 equation-set) (equation-set2 equation-set))
  )






