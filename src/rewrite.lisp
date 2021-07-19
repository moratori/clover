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
    :rewrite-final
    :find-critical-pair
    :all-critical-pair
    :rewrite-rule<
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
               (some
                 (lambda (v)
                   (find-if 
                     (lambda (u)
                       (term= (unifier.src u) v))
                     (unifier-set.unifiers unifset)))
                 variables)))
        (if is-error
            fterm
            (apply-unifier-set dst unifset)))
    (ununifiable-error (c) fterm)))


(defmethod %apply-rewrite-rule ((term term) (rewrite-rule rewrite-rule))
  (let* ((rule (rename rewrite-rule))
         (src (rewrite-rule.src rule))
         (dst (rewrite-rule.dst rule)))
    (%rewrite term src dst)))


(defmethod rewrite-final ((term vterm) (rewrite-rule rewrite-rule))
  (%apply-rewrite-rule term rewrite-rule))

(defmethod rewrite-final ((term fterm) (rewrite-rule rewrite-rule))
  (let ((applied
          (%apply-rewrite-rule
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



(defun copy-variant (original index variants)
  (unless (null original) 
    (mapcar
      (lambda (x)
        (loop
          :for elm :in original
          :for ind :from 0
          :collect
          (if (= ind index) x elm)))
      variants)))


(defmethod find-critical-pair ((term term) (rewrite-rule rewrite-rule))
  nil)

(defmethod find-critical-pair ((term constant) (rewrite-rule rewrite-rule))
  nil)

(defmethod find-critical-pair ((term fterm) (rewrite-rule rewrite-rule))
  (let* ((args (fterm.args term))
         (fsymbol (fterm.fsymbol term))
         (terms-make-from-args
             (loop
               :for arg :in args
               :for idx :from 0
               :for pairs   := (find-critical-pair arg rewrite-rule)
               :for variant := (copy-variant args idx pairs)
               :append (mapcar (lambda (x) (fterm fsymbol x)) variant)))
         (rewroted
           (%apply-rewrite-rule term rewrite-rule))
         (terms-make-from-toplevel
           (unless (term= rewroted term)
             (list rewroted))))
    (remove-duplicates
      (append 
        terms-make-from-toplevel
        terms-make-from-args)
      :test #'term=)))

(defmethod find-critical-pair ((term term) (rewrite-rule-set rewrite-rule-set))
  (remove-duplicates
    (mapcan
      (lambda (rule)
        (find-critical-pair term rule))
      (rewrite-rule-set.rewrite-rules rewrite-rule-set))
    :test #'term=))


(defmethod all-critical-pair ((rewrite-rule-set rewrite-rule-set))
  (let* ((rules
           (rewrite-rule-set.rewrite-rules rewrite-rule-set))
         (pairs-list
           (mapcar
             (lambda (rule)
               (find-critical-pair
                 (rewrite-rule.src rule)
                 rewrite-rule-set))
             rules))
         (combinate
           (mapcan
             (lambda (pair-candidate)
               (loop
                 :for term1 :in pair-candidate
                 :for i :from 0
                 :append
                 (loop 
                   :for term2 :in pair-candidate
                   :for j :from 0
                   :if (> j i)
                   :collect
                   (equation nil term1 term2))))
             pairs-list)))
    (equation-set
      (remove-duplicates
        (remove-if 
          #'tautology-equation-p
          combinate)
        :test #'equation=))))  



(defmethod rewrite-rule< ((rule1 rewrite-rule) (rule2 rewrite-rule))
  (let ((rule1-src (rewrite-rule.src rule1))
        (rule1-dst (rewrite-rule.dst rule1))
        (rule2-src (rewrite-rule.src rule2))
        (rule2-dst (rewrite-rule.dst rule2)))
    (or
      (not (rewrite-rule= rule1 rule2))
      (term< rule1-src rule2-src *term-order-algorithm*)
      (and 
        (alphabet= rule1-src rule2-src)
        (term< rule1-dst rule2-dst *term-order-algorithm*)))))

