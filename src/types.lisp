(defpackage clover.types
  (:use :cl
        :iddfs
        :clover.conditions
        )
  (:export
        :clause-set
        :clause-set.clauses
        :clause
        :clause.exprs
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
(in-package :clover.types)


(defun %%clause-set (obj)
  (and 
    (listp obj)
    (every (lambda (x)
             (typep x 'clause))
           obj)))

(deftype %clause-set ()
  '(satisfies %%clause-set))


(defun %%clause (obj)
  (and 
    (listp obj)
    (every (lambda (x)
             (typep x 'expr))
           obj)))

(deftype %clause ()
  '(satisfies %%clause))


(defun %%unifier-set (obj)
  (and 
    (listp obj)
    (every (lambda (x)
             (typep x 'unifier))
           obj)))

(deftype %unifier-set ()
  '(satisfies %%unifier-set))


(defun %%args (obj)
  (or (null obj)
      (and 
          (listp obj)
          (every (lambda (x) (typep x 'term)) obj))))

(deftype %args ()
  '(satisfies %%args))





(defstruct (clause-set
             (:include abstract-node)
             (:constructor clause-set (clauses))
             (:conc-name clause-set.))
  "節集合を表現する構造体
   節のリストを保持する"
   (clauses nil :type %clause-set))

(defstruct (clause
             (:constructor clause (exprs))
             (:conc-name clause.))
  "節を表現する構造体
   基本論理式のリストを保持する構造体"
   (exprs nil :type %clause))

(defstruct (expr
             (:constructor expr (negation predicate args))
             (:conc-name expr.))
  "基本論理式を表現する構造体
   否定の有無、述語名、述語の引数を保持する"
   (negation  nil :type boolean)
   (predicate nil :type symbol)
   (args      nil :type %args))


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
   (args    (error "default value required") :type %args))


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
  (unifiers nil :type %unifier-set))

