(defpackage clover.rename
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        )
  (:import-from :clover.parser
                :%intern-symbol-to-specified-package)
  (:export
    :rename
    :collect-variables
    :rename-for-human-readable-printing))
(in-package :clover.rename)


(defmethod collect-variables ((vterm vterm))
  (list vterm))

(defmethod collect-variables ((fterm fterm))
  (mapcan
    #'collect-variables
    (fterm.args fterm)))

(defmethod collect-variables ((literal literal))
  (mapcan
    #'collect-variables
    (literal.args literal)))

(defmethod make-rename-binds ((clause clause))
  (unifier-set
    (mapcar 
      (lambda (term)
        (unifier 
          term (vterm (gensym *vterm-gensym-prefix*))))
      (remove-duplicates
        (mapcan
          #'collect-variables
          (clause.literals clause))
        :test #'term=))))

(defmethod rename ((rewrite-rule rewrite-rule))
  (let* ((src (rewrite-rule.src rewrite-rule))
         (dst (rewrite-rule.dst rewrite-rule))
         (src-terms 
           (collect-variables src))
         (dst-terms 
           (collect-variables dst))
         (unique-terms
           (remove-duplicates 
             (append src-terms dst-terms)
             :test #'term=))
         (bind
           (unifier-set
             (mapcar 
               (lambda (x) (unifier x (vterm (gensym *vterm-gensym-prefix*))))
               unique-terms))))
    (rewrite-rule
      (apply-unifier-set src bind)
      (apply-unifier-set dst bind))))

(defmethod rename ((clause clause))
  (apply-unifier-set
    clause
    (make-rename-binds clause)))

(defmethod rename ((clause-set clause-set))
  (clause-set
    (mapcar 
      #'rename
      (clause-set.clauses clause-set))))


(defmethod rename-for-human-readable-printing ((clause clause))
  (let* ((unifier-set (make-rename-binds clause))
         (unif (unifier-set.unifiers unifier-set)))
    (cond 
      ((> (length unif) (length *vterm-for-human-readable*))
       ;; 変数の数が多すぎるので変換せずに返却
       clause)
      (t 
       (let ((readable-bind
               (unifier-set
                 (loop :for u :in unif
                       :for sym :in *vterm-for-human-readable*
                       :collect
                       (unifier
                         (unifier.src u)
                         (vterm (%intern-symbol-to-specified-package sym)))))))
         (apply-unifier-set
           clause
           readable-bind))))))

