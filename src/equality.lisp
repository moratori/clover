(defpackage clover.equality
  (:use :cl
        :clover.types)
  (:export
    :term=
    :term/=
    :unifier=
    :unifier-set=
    :literal=
    :clause=
    :clause-set=
    :equation-set=
    :equation=
    :rewrite-rule=
    :rewrite-rule-set=
    ))
(in-package :clover.equality)

(defmethod term/= ((obj1 t) (obj2 t))
  (not (term= obj1 obj2)))

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

(defmethod equation= ((equation1 equation) (equation2 equation))
  (let ((left1 (equation.left equation1))
        (left2 (equation.left equation2))
        (right1 (equation.right equation1))
        (right2 (equation.right equation2)))
    (and 
      (eq (equation.negation equation1)
          (equation.negation equation2))
      (or
        (and (term= left1 left2) (term= right1 right2))
        (and (term= left1 right2) (term= right1 left2))))))

(defmethod equation-set= ((equation-set1 equation-set) (equation-set2 equation-set))
  (and 
    (null (set-difference 
            (equation-set.equations equation-set1)
            (equation-set.equations equation-set2)
            :test #'equation=))
    (null (set-difference 
            (equation-set.equations equation-set2)
            (equation-set.equations equation-set1)
            :test #'equation=))))

(defmethod rewrite-rule= ((rule1 rewrite-rule) (rule2 rewrite-rule))
  (let ((rule1-src (rewrite-rule.src rule1))
        (rule1-dst (rewrite-rule.dst rule1))
        (rule2-src (rewrite-rule.src rule2))
        (rule2-dst (rewrite-rule.dst rule2)))
    (and 
      (term= rule1-src rule2-src)
      (term= rule1-dst rule2-dst))))

(defmethod rewrite-rule-set= ((rewrite-rule-set1 rewrite-rule-set) 
                              (rewrite-rule-set2 rewrite-rule-set))
  (and 
    (null (set-difference 
            (rewrite-rule-set.rewrite-rules rewrite-rule-set1)
            (rewrite-rule-set.rewrite-rules rewrite-rule-set2)
            :test #'rewrite-rule=))
    (null (set-difference
            (rewrite-rule-set.rewrite-rules rewrite-rule-set2)
            (rewrite-rule-set.rewrite-rules rewrite-rule-set1)
            :test #'rewrite-rule=))))
 
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
