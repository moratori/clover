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


(defmethod %make-rename-binds ((vterm vterm))
  (list vterm))

(defmethod %make-rename-binds ((fterm fterm))
  (mapcan
    #'%make-rename-binds
    (fterm.args fterm)))

(defmethod %make-rename-binds ((literal literal))
  (mapcan
    #'%make-rename-binds
    (literal.args literal)))

(defmethod %make-rename-binds ((clause clause))
  (unifier-set
    (mapcar 
      (lambda (term)
        (unifier 
          term (vterm (gensym *vterm-gensym-prefix*))))
      (remove-duplicates
        (mapcan
          #'%make-rename-binds
          (clause.literals clause))
        :test #'term=))))


(defmethod rename ((clause clause))
  (apply-unifier-set
    clause
    (%make-rename-binds clause)))

(defmethod rename ((clause-set clause-set))
  (clause-set
    (mapcar 
      #'rename
      (clause-set.clauses clause-set))))


(defmethod rename-for-human-readable-printing ((clause clause))
  (let* ((unifier-set (%make-rename-binds clause))
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

