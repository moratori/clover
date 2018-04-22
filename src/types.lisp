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
        :term
        :vterm
        :vterm.var
        :fterm
        :fterm.fsymbol
        :fterm.args
        :unifier.src
        :unifier.dst
        :unifier-set
        :unifier-set.unifiers

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


(defstruct term)

(defstruct (vterm
             (:include term)
             (:constructor vterm (var))
             (:conc-name vterm.))
  "変数項、定数項を表現する構造体
   シンボルを保持する"
   (var (error "default value required") :type symbol))

(defstruct (fterm
             (:include term)
             (:constructor fterm (fsymbol args))
             (:conc-name fterm.))
  "関数項を表現する構造体
   関数項のシンボルと引数を保持する"
   (fsymbol (error "default value required") :type symbol)
   (args (error "default value required") :type list))


(defstruct (unifier
             (:constructor unifier (src dst))
             (:conc-name unifier.))
  "単一化子を保持する構造体
   書き換え元、書き換え先を保持する"
   (src (error "default value required") :type vterm)
   (dst (error "default value required") :type term))


(defstruct (unifier-set
             (:constructor unifier-set (unifiers))
             (:conc-name unifier-set.))
  (unifiers nil :type list))

