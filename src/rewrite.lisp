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
    :find-critical-pair
    :all-critical-pair
    :apply-rewrite-rule
    :apply-rewrite-rule-set
    :rewrite-final
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
  (let* ((fsymbol1 (fterm.fsymbol fterm))
         (args1    (fterm.args fterm))
         (arity1   (length args1))
         (fsymbol2 (fterm.fsymbol src))
         (args2    (fterm.args src))
         (arity2   (length args2))
         (rewrited-args (mapcar (lambda (x) (%rewrite x src dst)) args1))
         (new (fterm fsymbol1 rewrited-args)))
    (handler-case
        (let* ((unifset
                ;; find-most-general-unifier-set は、元々導出の為の実装であり、変数同士から
                ;; unifierを作るときの順序は適当である。(左に与えられた項の変数が unifierのsrcになる)
                ;; 例: find-most-general-unifier-set(f(x), f(z)) -> {x -> z}
                ;; rewriteでは、src(書き換え規則)に含まれる変数をsrcにしたいので
                ;; src, new の順序にする
                 (find-most-general-unifier-set src new))
               (new-variables
                 (collect-variables new))
               (valid-unifset
                 (remove-if
                   (lambda (u)
                     (some
                       (lambda (v)
                         (term= v (unifier.src u)))
                       new-variables))
                   (unifier-set.unifiers unifset)))
               (invalid-unifset-cand
                 (remove-if-not
                   (lambda (u)
                     (some
                       (lambda (v)
                         (term= v (unifier.src u)))
                       new-variables))
                   (unifier-set.unifiers unifset)))
               (is-error
                 (some 
                   (lambda (u)
                     (typep (unifier.dst u) 'fterm))
                   invalid-unifset-cand))
               (corrected
                 (unless is-error
                   (unifier-set
                     (append 
                       valid-unifset
                       (mapcar
                         (lambda (u)
                           (let ((src (unifier.src x))
                                 (dst (unifier.dst x)))
                             (if (member src new-variables :test #'term=)
                                 (unifier dst src)
                                 u)))
                         invalid-unifset-cand))))))
          (if is-error
              new
              (apply-unifier-set dst corrected)))
        (ununifiable-error (c) new))))


(defmethod find-critical-pair ((term term) (rule1 rewrite-rule) (rule2 rewrite-rule))
  (let ((rule1-rewroted
          (apply-rewrite-rule term rule1))
        (rule2-rewroted
          (apply-rewrite-rule term rule2)))
    (unless (term= rule1-rewroted rule2-rewroted)
      (equation nil rule1-rewroted rule2-rewroted))))


(defmethod all-critical-pair ((rewrite-rule-set rewrite-rule-set))
  (let ((rules
          (rewrite-rule-set.rewrite-rules rewrite-rule-set)))
    (remove-duplicates
      (loop 
        :for rule1 :in rules
        :for i :from 0
        :append
        (loop
          :for rule2 :in rules
          :for j :from 0
          :if (/= i j)
          :append
          (let* ((rule1-src (rewrite-rule.src rule1))
                 (rule1-dst (rewrite-rule.dst rule1))
                 (rule2-src (rewrite-rule.src rule2))
                 (rule2-dst (rewrite-rule.dst rule2))
                 (pairs (list (find-critical-pair rule1-src rule1 rule2)
                              (find-critical-pair rule2-src rule1 rule2))))
            (remove-if #'null pairs))))
      :test #'equation=)))


(defmethod apply-rewrite-rule ((term term) (rewrite-rule rewrite-rule))
  (let* ((rule (rename rewrite-rule))
         (src (rewrite-rule.src rule))
         (dst (rewrite-rule.dst rule)))
    (%rewrite term src dst)))

(defmethod apply-rewrite-rule-set ((term term) (rewrite-rule-set rewrite-rule-set))
  (reduce 
    #'apply-rewrite-rule
    (rewrite-rule-set.rewrite-rules rewrite-rule-set)
    :initial-value term))


(defmethod rewrite-final ((term term) (rewrite-rule-set rewrite-rule-set))
  (let ((applied (apply-rewrite-rule-set term rewrite-rule-set)))
    (if (term= applied term)
        applied
        (rewrite-final applied rewrite-rule-set))))


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
