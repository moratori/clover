(defpackage clover.simplify
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        )
  (:export
    :simplify
    )
  )
(in-package :clover.simplify)


(defmethod %remove-duplicates-literal ((clause clause))
  (clause
    (remove-duplicates
      (clause.literals clause)
      :test #'literal=)
    (clause.parent1  clause)
    (clause.parent2  clause)
    (clause.focus-literal clause)
    (clause.unifier clause)
    (clause.clause-type clause)))


(defmethod %include-law-of-exclude-middle-p ((clause clause))
  (let ((literals (clause.literals clause)))
    (some 
      (lambda (lit1)
        (some 
          (lambda (lit2)
            (complement-literal-p lit1 lit2))
          literals))
      literals)))


(defun %remove-independent-clause (clauses)
  (let* ((predicates
           (remove-duplicates
             (mapcan
               (lambda (c)
                 (mapcar 
                   (lambda (l)
                     (cons (literal.negation l)
                           (literal.predicate l)))
                   (clause.literals c)))
               clauses)
             :test 
             (lambda (x y)
               (and (eq (car x) (car y))
                    (eq (cdr x) (cdr y))))))
         (elim-target-predicates
           (remove-if
             (lambda (p1)
               (destructuring-bind (neg1 . pred1) p1
                 (some
                   (lambda (p2)
                     (destructuring-bind (neg2 . pred2) p2
                       (and 
                         (not (eq neg1 neg2))
                         (eq pred1 pred2))))
                   predicates)))
             predicates)))
    (reduce
      (lambda (result-clauses elim-target)
        (destructuring-bind (neg . pred) elim-target
          (remove-if
            (lambda (c)
              (some
                (lambda (l)
                  (and 
                    (eq neg (literal.negation l))
                    (eq pred (literal.predicate l))))
                (clause.literals c)))
            result-clauses)))
      elim-target-predicates
      :initial-value clauses)))

(defun %remove-subsumption (clauses)
  (loop 
    :for target-clause :in clauses
    :for i :from 0
    :for subsumptioned := 
    (loop
      :named exit
      :for clause :in clauses
      :for j :from 0
      :if (and (> j i)
               (subsumption-clause-p clause target-clause))
      :do (return-from exit t))
    :if (not subsumptioned)
    :collect target-clause))

(defun %remove-alphabet-equal-clause (clauses)
  (loop 
    :for target-clause :in clauses
    :for i :from 0
    :for alphabet-clause := 
    (loop
      :named exit
      :for clause :in clauses
      :for j :from 0
      :if (and (> j i)
               (alphabet-clause= clause target-clause))
      :do (return-from exit t))
    :if (not alphabet-clause)
    :collect target-clause))


(defmethod simplify ((clause clause))
  (%remove-duplicates-literal clause))

(defmethod simplify ((clause-set clause-set))
  (let* ((clauses 
           (clause-set.clauses clause-set))
         (next-clauses ;; 重複リテラルの削除
           (mapcar #'simplify clauses))
         (next-clauses ;; トートロジーを含む節の削除
           (remove-if #'%include-law-of-exclude-middle-p next-clauses))
         (next-clauses ;; subsumption
           (%remove-subsumption next-clauses))
         (next-clauses ;; アルファベット同値な節ペアのうち1つを削除
           (%remove-alphabet-equal-clause next-clauses))
         (next-clauses ;; 単一述語の除去
           (%remove-independent-clause next-clauses)))
    (clause-set next-clauses)))

