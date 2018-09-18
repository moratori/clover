(defpackage clover.util
  (:use :cl
        :clover.conditions
        :clover.types)
  (:export
    :term=
    :unifier=
    :unifier-set=
    :literal=
    :complement-literal-p
    :resolution-candidate=
    :clause=)
  )
(in-package :clover.util)


(defmethod term= ((obj1 t) (obj2 t))
  nil)

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

(defmethod literal= ((literal1 literal) (literal2 literal))
  (let ((pred1 (literal.predicate literal1))
        (pred2 (literal.predicate literal2))
        (args1    (literal.args      literal1))
        (args2    (literal.args      literal2)))
    (and 
      (eq (literal.negation literal1) (literal.negation literal2))
      (eq pred1 pred2)
      (= (length args1) (length args2))
      (every #'term= args1 args2))))


(defmethod complement-literal-p ((literal1 literal) (literal2 literal))
  (let ((pred1 (literal.predicate literal1))
        (pred2 (literal.predicate literal2))
        (args1    (literal.args literal1))
        (args2    (literal.args literal2))
        (negation1 (literal.negation literal1))
        (negation2 (literal.negation literal2)))
    (and
      (not (eq negation1 negation2))
      (eq pred1 pred2)
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


(defmethod clause= ((clause1 clause) (clause2 clause))
  (and 
    (null (set-difference 
            (clause.literals clause1)
            (clause.literals clause2)
            :test #'literal=))
    (null (set-difference 
            (clause.literals clause2)
            (clause.literals clause1)
            :test #'literal=))))


(defmethod resolution-candidate= ((res-cand1 resolution-candidate) (res-cand2 resolution-candidate))
  (let ((focus-literal1 (res-cand.focus-literal res-cand1))
        (focus-literal2 (res-cand.focus-literal res-cand2))
        (cand-unifiers1 (res-cand.cand-unifiers res-cand1))
        (cand-unifiers2 (res-cand.cand-unifiers res-cand2)))
    (and 
      (literal= focus-literal1 focus-literal2)
      (null (set-difference 
              cand-unifiers1
              cand-unifiers2
              :test #'unifier-set=))
      (null (set-difference 
              cand-unifiers2
              cand-unifiers1
              :test #'unifier-set=)))))

