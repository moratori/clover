(defpackage clover.util
  (:use :cl
        :clover.conditions
        :clover.types)
  (:export
    :term=
    :unifier=
    :unifier-set=)
  )
(in-package :clover.util)



(defmethod term= ((term1 vterm) (term2 vterm))
  (eq (vterm.var term1) (vterm.var term2)))


(defmethod term= ((fterm1 fterm) (fterm2 fterm))
  (let ((fsymbol1 (fterm.fsymbol fterm1))
        (fsymbol2 (fterm.fsymbol fterm2))
        (args1    (fterm.args fterm1))
        (args2    (fterm.args fterm2)))
    (and 
      (eq fsymbol1 fsymbol2)
      (= (length args1) (length args2))
      (every #'term= args1 args2))))


(defmethod unifier= ((unifier1 unifier) (unifier2 unifier))
  (and 
    (term= (unifier.src unifier1) (unifier.src unifier2))
    (term= (unifier.dst unifier1) (unifier.dst unifier2))))


(defmethod unifier-set= ((unifier-set1 unifier-set) (unifier-set2 unifier-set))
  (and 
    (null (set-difference 
            (unifier-set.unifiers unifier-set1)
            (unifier-set.unifiers unifier-set2)
            :test #'unifier=))
    (null (set-difference 
            (unifier-set.unifiers unifier-set2)
            (unifier-set.unifiers unifier-set1)
            :test #'unifier=))))
