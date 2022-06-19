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
    :horn-p
    :goal-clause-p
    :rule-clause-p
    :fact-clause-p
    :clause-subset
    :term=
    :term/=
    :unifier=
    :unifier-set=
    :literal=
    :clause=
    :clause-set=
    :tautology-p
    :conseq-clause-p
    :ground-term-p
    :equation-set=
    :equation=
    :rewrite-rule=
    :rewrite-rule-set=
    :prohibited-unifier-set-p
    :occurrence-check
    :collect-variables
    ))
(in-package :clover.util)


(defmethod occurrence-check ((term1 term) (term2 term))
  nil)

(defmethod occurrence-check ((term1 vterm) (term2 vterm))
  (term= term1 term2))

(defmethod occurrence-check ((term1 vterm) (term2 fterm))
  (some
    (lambda (arg)
      (occurrence-check term1 arg))
    (fterm.args term2))) 

(defmethod prohibited-unifier-set-p ((unifier-set unifier-set) prohibited-variable-list)
  (some
    (lambda (unif)
      (member 
        (unifier.src unif) 
        prohibited-variable-list :test #'term=))
    (unifier-set.unifiers unifier-set)))

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

(defmethod tautology-p ((equation equation))
  (term= (equation.left equation)
         (equation.right equation)))

(defmethod tautology-p ((rewrite-rule rewrite-rule))
  (term= (rewrite-rule.src rewrite-rule)
         (rewrite-rule.dst rewrite-rule)))

(defmethod ground-term-p ((term vterm))
  nil)

(defmethod ground-term-p ((term constant))
  t)

(defmethod ground-term-p ((term fterm))
  (every #'ground-term-p (fterm.args term)))


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
      (= (length args1) (length args2))
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
              (or (term/= src (unifier.src target))
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

(defmethod horn-p ((clause clause))
  (<= (count-if 
        (lambda (lit)
          (eq (literal.negation lit) nil))
        (clause.literals clause)) 1))

(defmethod horn-p ((clause-set clause-set))
  (every #'horn-p (clause-set.clauses clause-set)))

(defmethod goal-clause-p ((clause clause))
  (every
    (lambda (lit)
      (eq (literal.negation lit) t))
    (clause.literals clause)))

(defmethod rule-clause-p ((clause clause))
  (let ((len (clause-length clause)))
    (and
      (= 1 
         (count-if
           (lambda (x) (eq (literal.negation x) nil))
           (clause.literals clause)))
      (= (1- len)
         (count-if
           (lambda (x) (eq (literal.negation x) t))
           (clause.literals clause))))))

(defmethod fact-clause-p ((clause clause))
  (and
    (= 1 (clause-length clause))
    (eq nil (literal.negation (first (clause.literals clause))))))

(defmethod conseq-clause-p ((clause clause))
  (eq (clause.clause-type clause) :conseq))


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

