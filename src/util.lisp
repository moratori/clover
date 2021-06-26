(defpackage clover.util
  (:use :cl
        :clover.conditions
        :clover.types)
  (:export
    :clause-length
    :complement-literal-p
    :has-parent-p
    :null-clause-p
    :consistent-unifier-set-p 
    :horn-clause-p
    :horn-clause-set-p
    :goal-clause-p
    :clause-subset
    :term=
    :unifier=
    :unifier-set=
    :literal=
    :clause=
    :clause-set=
    :conseq-clause-p
    ))
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

(defmethod null-clause-p ((clause clause))
  (null (clause.literals clause)))


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

(defmethod consistent-unifier-set-p ((unifier-set unifier-set))
  ;; どのunifier A Bをとっても、A.src = B.src　ならば A.dst = B.dst
  (let ((unifiers (unifier-set.unifiers unifier-set)))
    (every 
      (lambda (unifier)
        (let ((src (unifier.src unifier))
              (dst (unifier.dst unifier)))
          (every
            (lambda (target)
              (or (not (term= src (unifier.src target)))
                  (term= dst (unifier.dst target))))
            unifiers)))
      unifiers)))

(defmethod clause-length ((clause clause))
  (length 
    (clause.literals clause)))

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

(defmethod clause-subset ((clause1 clause) (clause2 clause))
  ;; clause1がclause2に含まれるか
  (let ((clause2-literals
          (clause.literals clause2)))
    (every 
      (lambda (lit1)
        (find lit1 clause2-literals
              :test #'literal=))
    (clause.literals clause1))))

(defmethod has-parent-p ((clause clause))
  (and (not (null (clause.parent1 clause)))
       (not (null (clause.parent2 clause)))))


(defmethod clause-set= ((clause-set1 clause-set) (clause-set2 clause-set))
  (and 
    (null (set-difference 
            (clause-set.clauses clause-set1)
            (clause-set.clauses clause-set2)
            :test #'clause=))
    (null (set-difference 
            (clause-set.clauses clause-set2)
            (clause-set.clauses clause-set1)
            :test #'clause=))))

(defmethod horn-clause-p ((clause clause))
  (<= (count-if 
        (lambda (lit)
          (eq (literal.negation lit) nil))
        (clause.literals clause)) 1))

(defmethod goal-clause-p ((clause clause))
  (every
    (lambda (lit)
      (eq (literal.negation lit) t))
    (clause.literals clause)))

(defmethod horn-clause-set-p ((clause-set clause-set))
  (every #'horn-clause-p (clause-set.clauses clause-set)))

(defmethod conseq-clause-p ((clause clause))
  (eq (clause.clause-type clause) :conseq))

