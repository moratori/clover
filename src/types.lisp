(defpackage types.clover
  (:use :cl)
  (:export
        :clause-set
        :clause-set.clauses
        :expr-set
        :expr-set.exprs
        :expr
        :expr.negation
        :expr.predicate
        :expr.args
  ))
(in-package :types.clover)


(defstruct (clause-set
                (:include abstract-node)
                (:constructor clause-set (clauses))
                (:conc-name clause-set.))
  "節集合を表現する構造体
   節のリストを保持する"
        (clauses nil :type list))

(defstruct (expr-set
                (:constructor expr-set (exprs))
                (:conc-name expr-set.))
  "節を表現する構造体
   基本論理式のリストを保持する構造体"
        (exprs nil :type list))

(defstruct (expr
                (:constructor expr (negation predicate args))
                (:conc-name expr.))
  "基本論理式を表現する構造体
   否定の有無、述語名、述語の引数を保持する"
        (negation  nil :type boolean)
        (predicate nil :type symbol)
        (args      nil :type list))
