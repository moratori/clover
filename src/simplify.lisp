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


(defmethod simplify ((clause clause))
  (%remove-duplicates-literal clause))

(defmethod simplify ((clause-set clause-set))
  (let* ((clauses 
           (clause-set.clauses clause-set))
         (next-clauses ;; 重複リテラルの削除
           (mapcar #'simplify clauses))
         (next-clauses ;; トートロジーを含む節の削除
           (remove-if #'%include-law-of-exclude-middle-p next-clauses))
         (eliminated-clause-list nil)
         (next-clauses ;; アルファベット同値な節ペアのうち1つを削除
           (remove-if 
             (lambda (c1)
               (some
                 (lambda (c2) 
                   (when (and (not (clause= c1 c2))
                              (alphabet-clause= c1 c2)
                              (not (member c1 eliminated-clause-list 
                                           :test #'alphabet-clause=)))
                     (setf eliminated-clause-list 
                           (push c1 eliminated-clause-list))
                     t))
                 next-clauses))
             next-clauses))
         (next-clauses ;; 単一述語の除去
           (%remove-independent-clause next-clauses)))
    (clause-set next-clauses)))

