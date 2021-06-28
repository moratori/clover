(defpackage clover.types
  (:use :cl
        :clover.search.common
        :clover.conditions
        )
  (:import-from :clover.property
                :*parsed-symbol-intern-package*)
  (:export
        :logical-expression
        :clause-set
        :clause-set.clauses
        :clause-set.resolution-mode
        :clause
        :clause.literals
        :clause.parent1
        :clause.parent2
        :clause.unifier
        :clause.clause-type
        :clause.used-cnt
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
        :equation
        :equation-set
        :equation-set.equations
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


(defun %%equation-set (obj)
  (and 
    (listp obj)
    (every (lambda (x)
             (typep x 'equation))
           obj)))

(deftype %equation-set ()
  '(satisfies %%equation-set))


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
      (eq obj :conseq)
      (eq obj :premise)
      (eq obj :center)
      (eq obj :resolvent))))

(deftype %clause-type ()
  '(satisfies %%clause-type))



(defstruct term)

(defstruct (vterm
             (:print-object
               (lambda (object stream)
                 (format stream "~A" (string-downcase (symbol-name (vterm.var object))))))
             (:include term)
             (:constructor vterm (var))
             (:conc-name vterm.))
  "変数項を表現する構造体
   シンボルを保持する"
   (var (error "default value required") :type symbol :read-only t))

(defstruct (fterm
             (:print-object 
               (lambda (object stream)
                 (let* ((fsymbol (fterm.fsymbol object))
                        (fsymbol-name (symbol-name fsymbol))
                        (args    (fterm.args object)))
                   (if (null args)
                     (format stream "~A" (string-upcase fsymbol-name))
                     (format stream "~A(~{~A~^,~})" (string-downcase fsymbol-name) args)))))
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
                   (format stream "~A~A(~{~A~^,~})"
                           (if negation "!" "")
                           (string-downcase (symbol-name predicate))
                           args))))
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
                    (format stream "~{~A ~^| ~}"
                         (clause.literals object))))))
             (:include logical-expression)
             (:constructor clause (literals &optional parent1 parent2 unifier clause-type used-cnt))
             (:conc-name clause.))
  "節を表現する構造体
   基本論理式のリストを保持する構造体"
   (literals nil :type %clause :read-only t)
   (parent1  nil :type (or null clause)  :read-only t)
   (parent2  nil :type (or null clause)  :read-only t)
   (unifier  nil :type (or null unifier-set) :read-only t)
   (clause-type :premise :type %clause-type :read-only t)
   (used-cnt 0 :type integer :read-only t))


(defstruct (clause-set
             (:print-object 
               (lambda (object stream)
                 (format stream "~{~A~%~}"
                         (clause-set.clauses object))))
             (:include abstract-node)
             (:constructor clause-set (clauses &optional resolution-mode))
             (:conc-name clause-set.))
  "節集合を表現する構造体
   節のリストを保持する"
   (clauses nil :type %clause-set :read-only t)
   (resolution-mode nil :type symbol :read-only t))

(defstruct (equation
             (:include literal)
             (:conc-name equation.)
             (:constructor equation 
              (negation left right &aux 
                        (predicate (intern "=" *parsed-symbol-intern-package*)) 
                        (args (list left right))))
             (:print-object
              (lambda (object stream)
                 (let ((negation (literal.negation object))
                       (args (literal.args object)))
                   (format stream "~A ~A ~A"
                           (first args)
                           (if negation "≠" "=")
                           (second args))))))
  "等式を表す構造体"
  )

(defstruct (equation-set
             (:print-object 
              (lambda (object stream)))
             (:constructor equation-set (equations))
             (:conc-name equation-set.))
  "等式の集合を表現する構造体"
  (equations nil :type %equation-set :read-only t))
