(defpackage clover.rename
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        )
  (:export
    :rename
    )
  )
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
