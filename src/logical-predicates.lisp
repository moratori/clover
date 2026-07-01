(defpackage clover.logical-predicates
  (:use :cl
        :clover.conditions
        :clover.types)
  (:import-from 
    :clover.equality
    :term=
    :term/=
    :literal=
  )
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
    :tautology-p
    :conseq-clause-p
    :ground-term-p
    :prohibited-unifier-set-p
    :occurrence-check
    :collect-variables
    ))
(in-package :clover.logical-predicates)


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

