(defpackage clover.types
  (:use :cl
        :clover.search.iddfs
        :clover.conditions
        )
  (:export
        :logical-expression
        :clause-set
        :clause-set.clauses
        :clause
        :clause.literals
        :clause.parent1
        :clause.parent2
        :clause.focus-literal
        :clause.unifier
        :clause.clause-type
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
        :res-cand.focus-literal
        :res-cand.cand-unifiers
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


(defun %%clause-type (obj)
  (and 
    (typep obj 'symbol)
    (or 
      (eq obj :fact)
      (eq obj :rule)
      (eq obj :goal)
      (eq obj :center)
      (eq obj :horn)
      (eq obj :premise)
      (eq obj :resolvent)
      (eq obj :inputted))))

(deftype %clause-type ()
  '(satisfies %%clause-type))



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
   (var (error "default value required") :type symbol :read-only t))

(defstruct (fterm
             (:print-object 
               (lambda (object stream)
                 (let ((fsymbol (fterm.fsymbol object))
                       (args    (fterm.args object)))
                   (if (null args)
                     (format stream "~A" fsymbol)
                     (format stream "~A(~{~A~^,~})" fsymbol args)))))
             (:include term)
             (:constructor fterm (fsymbol args))
             (:conc-name fterm.))
  "関数項を表現する構造体
   関数項のシンボルと引数を保持する"
   (fsymbol (error "default value required") :type symbol :read-only t)
   (args    (error "default value required") :type %args  :read-only t))


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
   (src (error "default value required") :type vterm :read-only t)
   (dst (error "default value required") :type term  :read-only t))


(defstruct (unifier-set
             (:print-object 
               (lambda (object stream)
                 (format stream "{~{~A~^,~}}" 
                         (unifier-set.unifiers object))))
             (:constructor unifier-set (unifiers))
             (:conc-name unifier-set.))
  (unifiers nil :type %unifier-set :read-only t))


(defstruct logical-expression)


(defstruct (literal
             (:print-object
               (lambda (object stream)
                 (let ((negation (literal.negation object))
                       (predicate (literal.predicate object))
                       (args (literal.args object)))
                   (if (null args)
                     (format stream "~A~A"
                         (if negation "!" "")
                         predicate)
                     (format stream "~A~A(~{~A~^,~})"
                         (if negation "!" "")
                         predicate
                         args)))))
             (:include logical-expression)
             (:constructor literal (negation predicate args))
             (:conc-name literal.))
  "基本論理式を表現する構造体
   否定の有無、述語名、述語の引数を保持する"
   (negation  nil :type boolean :read-only t)
   (predicate nil :type symbol  :read-only t)
   (args      nil :type %args   :read-only t))


(defstruct (clause
             (:print-object
               (lambda (object stream)
                 (cond 
                   ((null (clause.literals object))
                    (format stream "□"))
                   (t
                    (format stream "~{~A ~^v ~}"
                         (clause.literals object))))))
             (:include logical-expression)
             (:constructor clause (literals &optional parent1 parent2 focus-literal unifier clause-type))
             (:conc-name clause.))
  "節を表現する構造体
   基本論理式のリストを保持する構造体"
   (literals nil :type %clause :read-only t)
   (parent1  nil :type (or null clause)  :read-only t)
   (parent2  nil :type (or null clause)  :read-only t)
   (focus-literal nil :type (or null literal) :read-only t)
   (unifier  nil :type (or null unifier-set) :read-only t)
   (clause-type :premise :type %clause-type :read-only t))


(defstruct (clause-set
             (:print-object 
               (lambda (object stream)
                 (format stream "{~{~A~^, ~}}"
                         (clause-set.clauses object))))
             (:include abstract-node)
             (:constructor clause-set (clauses))
             (:conc-name clause-set.))
  "節集合を表現する構造体
   節のリストを保持する"
   (clauses nil :type %clause-set :read-only t))


