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
    :rewrite-all-ways
    :all-critical-pair
    :find-subterms
    :rewrite-rule<
    )
  )
(in-package :clover.rewrite)


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






(defmethod find-subterms ((term1 vterm) (term2 constant))
  nil)

(defmethod find-subterms ((term1 fterm) (term2 constant))
  nil)

(defmethod find-subterms ((term1 vterm) (term2 vterm))
  (list term1))

(defmethod find-subterms ((term1 constant) (term2 vterm))
  (list term1))

(defmethod find-subterms ((term1 constant) (term2 constant))
  (list term1)) 

(defmethod find-subterms ((term1 fterm) (term2 vterm))
  (list term1))

(defmethod find-subterms ((term1 term) (term2 fterm))
  (labels
      ((recursive-mgu (term target)
         (typecase target
           (fterm
             (let* ((unifset-from-toplevel
                      (typecase term
                        (fterm 
                          (handler-case
                              (find-most-general-unifier-set target term)
                            (ununifiable-error (c) nil)))
                        (t nil)))
                    (prohibited-variables
                      (collect-variables term))
                    (is-error
                      (when unifset-from-toplevel
                        (prohibited-unifier-set-p
                          unifset-from-toplevel
                          prohibited-variables)))
                    (unifset-from-args
                      (mapcan
                        (lambda (arg)
                          (recursive-mgu term arg))
                        (fterm.args target))))
               (if (and unifset-from-toplevel (not is-error))
                   (cons unifset-from-toplevel unifset-from-args)
                   unifset-from-args)))
           (vterm 
             nil
             ;(unless (occurrence-check target term)
             ;  (list (unifier-set
             ;          (list (unifier target term)))))
             )
           (constant
             nil)
           (t (error "~%target ~A must be term type~%" target)))))
    (let ((unifiers (recursive-mgu term1 term2)))
      (remove-duplicates
        (mapcar 
          (lambda (unifset)
            (apply-unifier-set term2 unifset))
          unifiers)
        :test #'term=))))


(defmethod all-critical-pair ((rewrite-rule-set rewrite-rule-set))
  (let* ((rules 
           (rewrite-rule-set.rewrite-rules rewrite-rule-set))
         (tmp
           (loop
             :for rule1 :in rules
             :for renamed-rule1 := (rename rule1)
             :append
             (loop 
               :for rule2 :in rules
               :for renamed-rule2 := (rename rule2)
               :append
               (let* ((rule1-src (rewrite-rule.src renamed-rule1))
                      (rule2-src (rewrite-rule.src renamed-rule2))
                      (subterms  (find-subterms rule1-src rule2-src)))
                 (mapcan
                   (lambda (term)
                     (let ((rewroted-by-rule1 (rewrite-all-ways term renamed-rule1))
                           (rewroted-by-rule2 (rewrite-all-ways term renamed-rule2)))
                       (loop
                         :for p1 :in rewroted-by-rule1
                         :append
                         (loop
                           :for p2 :in rewroted-by-rule2
                           :collect (equation nil p1 p2)))))
                   subterms))))))
    (equation-set
      (remove-duplicates
        (remove-if
          #'tautology-equation-p
          tmp)
        :test #'equation=))))


