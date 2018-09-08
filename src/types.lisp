(defpackage clover.types
  (:use :cl
        :iddfs
        :clover.conditions
        )
  (:export
        :clause-set
        :clause-set.clauses
        :clause
        :clause.literals
        :literal
        :literal.negation
        :literal.predicate
        :literal.args
        :term
        :vterm
        :vterm.var
        :fterm
        :fterm.fsymbol
        :fterm.args
        :unifier
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
             (typep x 'literal))
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
             (:print-object 
               (lambda (object stream)
                 (format stream "{~{~A~^,~}}"
                         (clause-set.clauses object))))
             (:include abstract-node)
             (:constructor clause-set (clauses))
             (:conc-name clause-set.))
  "節集合を表現する構造体
   節のリストを保持する"
   (clauses nil :type %clause-set))

(defstruct (clause
             (:print-object
               (lambda (object stream)
                 (format stream "~{~A ~^v~}"
                         (clause.literals object))))
             (:constructor clause (literals))
             (:conc-name clause.))
  "節を表現する構造体
   基本論理式のリストを保持する構造体"
   (literals nil :type %clause))

(defstruct (literal
             (:print-object
               (lambda (object stream)
                 (format stream "!~A(~{~A~^,~})" 
                         (literal.negation object)
                         (literal.predicate object)
                         (literal.args object))))
             (:constructor literal (negation predicate args))
             (:conc-name literal.))
  "基本論理式を表現する構造体
   否定の有無、述語名、述語の引数を保持する"
   (negation  nil :type boolean)
   (predicate nil :type symbol)
   (args      nil :type %args))


(defstruct term)

(defstruct (vterm
             (:print-object
               (lambda (object stream)
                 (format stream "~A" (vterm.var object))))
             (:include term)
             (:constructor vterm (var))
             (:conc-name vterm.))
  "変数項、定数項を表現する構造体
   シンボルを保持する"
   (var (error "default value required") :type symbol))

(defstruct (fterm
             (:print-object 
               (lambda (object stream)
                 (format stream "~A(~{~A~^,~})" 
                         (fterm.fsymbol object)
                         (fterm.args object))))
             (:include term)
             (:constructor fterm (fsymbol args))
             (:conc-name fterm.))
  "関数項を表現する構造体
   関数項のシンボルと引数を保持する"
   (fsymbol (error "default value required") :type symbol)
   (args    (error "default value required") :type %args))


(defstruct (unifier
             (:print-object 
               (lambda (object stream)
                 (format stream "~A->~A" 
                         (unifier.src object)
                         (unifier.dst object))))
             (:constructor unifier (src dst))
             (:conc-name unifier.))
  "単一化子を保持する構造体
   書き換え元、書き換え先を保持する"
   (src (error "default value required") :type vterm)
   (dst (error "default value required") :type term))


(defstruct (unifier-set
             (:print-object 
               (lambda (object stream)
                 (format stream "{~{~A~^,~}}" 
                         (unifier-set.unifiers object))))
             (:constructor unifier-set (unifiers))
             (:conc-name unifier-set.))
  (unifiers nil :type %unifier-set))

