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
    :rename-for-human-readable-printing))
(in-package :clover.rename)


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

(defmethod make-rename-binds ((rewrite-rule rewrite-rule))
  (unifier-set
    (mapcar 
      (lambda (term)
        (unifier 
          term (vterm (gensym *vterm-gensym-prefix*))))
      (remove-duplicates
        (mapcan
          #'collect-variables
          (list 
            (rewrite-rule.src rewrite-rule)
            (rewrite-rule.dst rewrite-rule)))
        :test #'term=))))

(defmethod make-rename-binds ((equation equation))
  (unifier-set
    (mapcar 
      (lambda (term)
        (unifier 
          term (vterm (gensym *vterm-gensym-prefix*))))
      (remove-duplicates
        (mapcan
          #'collect-variables
          (list 
            (equation.left equation)
            (equation.right equation)))
        :test #'term=))))

(defmethod rename ((equation equation))
  (let ((rename-binds
          (make-rename-binds equation)))
    (equation
      (equation.negation equation)
      (apply-unifier-set 
        (equation.left equation) 
        rename-binds)
      (apply-unifier-set 
        (equation.right equation) 
        rename-binds))))

(defmethod rename ((rewrite-rule rewrite-rule))
  (let* ((rename-binds
           (make-rename-binds rewrite-rule)))
    (rewrite-rule
      (apply-unifier-set
        (rewrite-rule.src rewrite-rule)
        rename-binds)
      (apply-unifier-set
        (rewrite-rule.dst rewrite-rule)
        rename-binds))))

(defmethod rename ((clause clause))
  (apply-unifier-set
    clause
    (make-rename-binds clause)))

(defmethod rename ((clause-set clause-set))
  (clause-set
    (mapcar 
      #'rename
      (clause-set.clauses clause-set))
    (clause-set.resolution-mode clause-set)))

(defmethod rename ((equation-set equation-set))
  (equation-set
    (mapcar 
      #'rename
      (equation-set.equations equation-set))))


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

(defmethod rename-for-human-readable-printing ((equation equation))
  (let* ((unifier-set (make-rename-binds equation))
         (unif (unifier-set.unifiers unifier-set)))
    (cond 
      ((> (length unif) (length *vterm-for-human-readable*))
       ;; 変数の数が多すぎるので変換せずに返却
       equation)
      (t 
       (let ((readable-bind
               (unifier-set
                 (loop :for u :in unif
                       :for sym :in *vterm-for-human-readable*
                       :collect
                       (unifier
                         (unifier.src u)
                         (vterm (%intern-symbol-to-specified-package sym)))))))
         (equation
           (equation.negation equation)
           (apply-unifier-set 
             (equation.left equation) readable-bind)
           (apply-unifier-set
             (equation.right equation) readable-bind)))))))

(defmethod rename-for-human-readable-printing ((equation-set equation-set))
  (equation-set
    (mapcar #'rename-for-human-readable-printing 
            (equation-set.equations equation-set))))

(defmethod rename-for-human-readable-printing ((rewrite-rule rewrite-rule))
  (let* ((unifier-set (make-rename-binds rewrite-rule))
         (unif (unifier-set.unifiers unifier-set)))
    (cond 
      ((> (length unif) (length *vterm-for-human-readable*))
       ;; 変数の数が多すぎるので変換せずに返却
       rewrite-rule)
      (t 
       (let ((readable-bind
               (unifier-set
                 (loop :for u :in unif
                       :for sym :in *vterm-for-human-readable*
                       :collect
                       (unifier
                         (unifier.src u)
                         (vterm (%intern-symbol-to-specified-package sym)))))))
         (rewrite-rule
           (apply-unifier-set 
             (rewrite-rule.src rewrite-rule) readable-bind)
           (apply-unifier-set
             (rewrite-rule.dst rewrite-rule) readable-bind)))))))

(defmethod rename-for-human-readable-printing ((rewrite-rule-set rewrite-rule-set))
  (rewrite-rule-set
    (mapcar #'rename-for-human-readable-printing 
            (rewrite-rule-set.rewrite-rules rewrite-rule-set))))
