(defpackage clover.rewrite
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        :clover.rename
        )
  (:import-from :clover.termorder
                :term<)
  (:export
    :rewrite
    :rewrite-final
    :rewrite-all-ways
    )
  )
(in-package :clover.rewrite)




(defmethod %rewrite ((vterm vterm) (src vterm) (dst vterm))
  vterm)

(defmethod %rewrite ((vterm vterm) (src vterm) (dst fterm))
  (apply-unifier
    dst
    (unifier src vterm)))

(defmethod %rewrite ((vterm vterm) (src fterm) (dst term))
  vterm)

(defmethod %rewrite ((fterm fterm) (src vterm) (dst term))
  (apply-unifier
    dst
    (unifier src fterm)))

(defmethod %rewrite ((fterm fterm) (src fterm) (dst term))
  (handler-case
      (let* ((unifset
;; find-most-general-unifier-set は、元々導出の為の実装であり、変数同士から
;; unifierを作るときの順序は適当である。(左に与えられた項の変数が unifierのsrcになる)
;; 例: find-most-general-unifier-set(f(x), f(z)) -> {x -> z}
;; rewriteでは、src(書き換え規則)に含まれる変数をsrcにしたいので
;; srcを左にとる  
               (find-most-general-unifier-set src fterm))
             (variables
               (collect-variables fterm))
             (is-error
               (prohibited-unifier-set-p unifset variables)))
        (if is-error
            fterm
            (apply-unifier-set dst unifset)))
    (ununifiable-error (c) fterm)))

(defmethod rewrite ((term term) (rewrite-rule rewrite-rule))
  (let ((renamed (rename rewrite-rule)))
    (%rewrite term
              (rewrite-rule.src renamed)
              (rewrite-rule.dst renamed))))



(defgeneric rewrite-final (target rewrite-rule)
  (:documentation 
   "rewrite-ruleを用いて、targetが書き換え不能になるまで書き換える。rewrite-ruleよにっては、停止しない可能性がある。"))


(defmethod rewrite-final ((term vterm) (rewrite-rule rewrite-rule))
  (rewrite term rewrite-rule))

(defmethod rewrite-final ((term fterm) (rewrite-rule rewrite-rule))
  (let ((applied
          (rewrite
            (fterm
              (fterm.fsymbol term)
              (mapcar
                (lambda (arg)
                  (rewrite-final arg rewrite-rule))
                (fterm.args term)))
            rewrite-rule)))
    (if (term= applied term)
        applied
        (rewrite-final applied rewrite-rule))))

(defmethod rewrite-final ((term term) (rewrite-rule-set rewrite-rule-set))
  (let ((applied 
          (reduce 
            #'rewrite-final
            (rewrite-rule-set.rewrite-rules rewrite-rule-set)
            :initial-value term)))
    (if (term= applied term)
        applied
        (rewrite-final applied rewrite-rule-set))))

(defmethod rewrite-final ((equation equation) (rewrite-rule-set rewrite-rule-set))
  (equation
    (equation.negation equation)
    (rewrite-final (equation.left equation) rewrite-rule-set)
    (rewrite-final (equation.right equation) rewrite-rule-set)))




(defun copy-variant (original index variants)
  "originalのindex目の要素をvariantsの要素で置き換えたもののリストを返す"
  (unless (null original) 
    (mapcar
      (lambda (x)
        (loop
          :for elm :in original
          :for ind :from 0
          :collect
          (if (= ind index) x elm)))
      variants)))

(defmethod rewrite-all-ways ((term term) (rewrite-rule rewrite-rule))
  nil)

(defmethod rewrite-all-ways ((term constant) (rewrite-rule rewrite-rule))
  nil)

(defmethod rewrite-all-ways ((term fterm) (rewrite-rule rewrite-rule))
  (let* ((args (fterm.args term))
         (fsymbol (fterm.fsymbol term))
         (terms-make-from-args
             (loop
               :for arg :in args
               :for idx :from 0
               :for pairs   := (rewrite-all-ways arg rewrite-rule)
               :for variant := (copy-variant args idx pairs)
               :append (mapcar (lambda (x) (fterm fsymbol x)) variant)))
         (rewroted
           (rewrite term rewrite-rule))
         (terms-make-from-toplevel
           (unless (term= rewroted term)
             (list rewroted))))
    (remove-duplicates
      (append 
        terms-make-from-toplevel
        terms-make-from-args)
      :test #'term=)))

(defmethod rewrite-all-ways ((term term) (rewrite-rule-set rewrite-rule-set))
  (remove-duplicates
    (mapcan
      (lambda (rule)
        (rewrite-all-ways term rule))
      (rewrite-rule-set.rewrite-rules rewrite-rule-set))
    :test #'term=))

