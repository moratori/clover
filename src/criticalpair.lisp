(defpackage clover.criticalpair
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        :clover.rename
        :clover.rewrite
        )
  (:import-from :clover.termorder
                :term<)
  (:export
    :all-critical-pair
    :find-subterms
    ))
(in-package :clover.criticalpair)



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
                    (unifset-from-args
                      (mapcan
                        (lambda (arg)
                          (recursive-mgu term arg))
                        (fterm.args target))))
               (if unifset-from-toplevel 
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
                           :collect (rename (equation nil p1 p2))))))
                   subterms))))))
    (equation-set
      (remove-duplicates
        (remove-if
          #'tautology-p
          tmp)
        :test (lambda (x y)
                (or (equation= x y)
                    (alphabet= x y)))))))
