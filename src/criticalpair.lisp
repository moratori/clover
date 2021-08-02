(defpackage clover.criticalpair
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
    :critical-pair
    :all-critical-pair
    ))
(in-package :clover.criticalpair)


(defgeneric critical-pair (rule1 rule2)
  (:documentation "https://gist.github.com/moratori/0c21dbafac0391f3aa0ee3f41a002f2f"))


(defmethod critical-pair ((rewrite-rule1 rewrite-rule) (rewrite-rule2 rewrite-rule))
  (let* ((rule1-src
           (rewrite-rule.src rewrite-rule1))
         (rule1-dst
           (rewrite-rule.dst rewrite-rule1))
         (rule2-src
           (rewrite-rule.src rewrite-rule2))
         (rule2-dst
           (rewrite-rule.dst rewrite-rule2))
         (theta-list nil))
    (labels
        ((recursive-mgu (small target)
           (typecase target
             (vterm nil)
             (constant nil)
             (fterm
               (let ((mgu-from-toplevel
                       (handler-case
                           (find-most-general-unifier-set
                             small target)
                           (ununifiable-error (c) nil)))
                     (mgu-from-args
                       (mapcan
                         (lambda (x)
                           (recursive-mgu small x))
                         (fterm.args target))))
                 (if mgu-from-toplevel
                     (cons mgu-from-toplevel mgu-from-args)
                     mgu-from-args)))))
         (substitute-all-subterm (theta src dst target)
           (let ((mgu 
                   (handler-case
                       (find-most-general-unifier-set src target)
                       (ununifiable-error (C) 
                         nil))))
             (if (and (not (typep target 'vterm))
                      mgu 
                      (unifier-set= mgu theta))
                 dst
                 (typecase target
                   (vterm target)
                   (constant (if (term= target src) dst target))
                   (fterm (fterm (fterm.fsymbol target)
                                 (mapcar
                                   (lambda (arg)
                                     (substitute-all-subterm theta src dst arg))
                                   (fterm.args target)))))))))
      (setf theta-list
            (remove-duplicates
              (recursive-mgu rule1-src rule2-src)
              :test #'unifier-set=))
      (equation-set
        (mapcar
          (lambda (theta)
            (let ((rewroted-by-rule2
                    (apply-unifier-set
                        rule2-dst
                        theta))
                    (another
                      (substitute-all-subterm
                        theta rule1-src rule1-dst rule2-src)))
                (rename
                  (equation
                    nil
                    rewroted-by-rule2
                    (apply-unifier-set another theta)))))
            theta-list)))))

(defmethod all-critical-pair ((rewrite-rule-set rewrite-rule-set))
  (let* ((rules
           (rewrite-rule-set.rewrite-rules
             rewrite-rule-set))
         (tmp
           (loop
             :for rule1 :in rules
             :for renamed-rule1 := (rename rule1)
             :append
             (loop
               :for rule2 :in rules
               :for renamed-rule2 := (rename rule2)
               :collect
               (critical-pair renamed-rule1 renamed-rule2))))
         (result
           (mapcan
             (lambda (x) 
               (equation-set.equations x))
             tmp)))
    (equation-set
      (remove-duplicates
        (remove-if
          #'tautology-p
          result)
        :test (lambda (x y)
                (or (equation= x y)
                    (alphabet= x y)))))))

