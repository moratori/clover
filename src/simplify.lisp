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
    (clause.unifier clause)
    (clause.clause-type clause)
    (clause.used-cnt clause)))


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
  ;; 各ペア (i<j) を1回だけ調べる。
  ;; - i が j を包摂（strict でも相互でも）→ 後ろの j を削除し、前の i を代表として残す。
  ;; - 前向きが不成立のときだけ逆向きを確認し、j が i を包摂すれば i を削除する。
  ;; これにより前向き呼び出しは各ペア1回（旧 (/= j i) 版の約半分）に減り、逆向き呼び出しは
  ;; 前向きが外れたペアのみとなる。removed フラグで既に削除済みの節を含むペアはスキップする。
  ;; 振る舞いは従来と同値: strict subsumption は順序非依存に削除し、相互包摂（変種・重複節）は
  ;; 最前の1つを代表として残す。
  (let* ((vec (coerce clauses 'vector))
         (n (length vec))
         (removed (make-array n :initial-element nil)))
    (loop :for i :below n :do
      (loop :for j :from (1+ i) :below n :do
        (unless (or (aref removed i) (aref removed j))
          (cond
            ((subsumption-clause-p (aref vec i) (aref vec j))
             (setf (aref removed j) t))
            ((subsumption-clause-p (aref vec j) (aref vec i))
             (setf (aref removed i) t))))))
    (loop :for i :below n
          :unless (aref removed i)
          :collect (aref vec i))))

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
               (alphabet= clause target-clause))
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
    (clause-set
      next-clauses
      (clause-set.resolution-mode clause-set))))

