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
        :vterm.original-str
        :fterm
        :fterm.fsymbol
        :fterm.args
        :constant
        :constant.value
        :equation
        :equation.negation
        :equation.left
        :equation.right
        :unifier
        :unifier.src
        :unifier.dst
        :unifier-set
        :unifier-set.unifiers
        :equation-set
        :equation-set.equations
        :rewrite-rule
        :rewrite-rule.src
        :rewrite-rule.dst
        :rewrite-rule-set
        :rewrite-rule-set.rewrite-rules
        :function-symbol-ordering
        :function-symbol-ordering.ordering
        :mkbtt-form
        :mkbtt-form.var
        :mkbtt-form.rules
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


(defun %%rewrite-rule-set (obj)
  (and
    (listp obj)
    (every (lambda (x)
             (typep x 'rewrite-rule))
           obj)))

(deftype %rewrite-rule-set ()
  '(satisfies %%rewrite-rule-set))


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

(defun %%function-symbol-ordering (obj)
  (or (null obj)
      (and
          (listp obj)
          (every (lambda (x) (typep x 'symbol)) obj))))

(deftype %function-symbol-ordering ()
  '(satisfies %%function-symbol-ordering))

(defun %%mkbtt-var-type (obj)
  (or (null obj)
      (and
          (listp obj)
          (every (lambda (x) (typep x 'vterm)) obj))))

(deftype %mkbtt-var-type ()
  '(satisfies %%mkbtt-var-type))


(defstruct term)

(defstruct (vterm
             (:print-object
               (lambda (object stream)
                 (format stream "~A" (string-downcase (symbol-name (vterm.var object))))))
             (:include term)
             (:constructor vterm (var &optional original-str))
             (:conc-name vterm.))
  "変数項を表現する構造体
   シンボルを保持する"
   (var (error "default value required") :type symbol :read-only t)
  ; mkbtt形式をパースする際に、大文字小文字を区別する必要があることから
  ; 元の文字列を保持する
   (original-str "" :type string :read-only t))

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

(defstruct (constant
             (:include fterm)
             (:print-object
              (lambda (object stream)
                (format stream "~A"
                        (string-upcase (symbol-name (constant.value object))))))
             (:constructor constant
              (fsymbol &aux (args nil)))
             (:conc-name constant.))
  "定数(引数なしのfterm)を表す構造体"
  (value fsymbol :type symbol)
  )

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
  "等式または危険対を表す構造体"
  left right
  )

(defstruct (equation-set
             (:print-object
              (lambda (object stream)
                (format stream "{~{~A~^,~}}"
                        (equation-set.equations object))))
             (:constructor equation-set (equations))
             (:conc-name equation-set.))
  "等式の集合を表現する構造体"
  (equations nil :type %equation-set :read-only t))


(defstruct (rewrite-rule
             (:print-object
               (lambda (object stream)
                 (format stream "~A=>~A"
                         (rewrite-rule.src object)
                         (rewrite-rule.dst object))))
             (:constructor rewrite-rule (src dst))
             (:conc-name rewrite-rule.))
  "書き換え規則を保持する。unifier構造体との差異は、srcにftermが来ることを許すこと"
   (src (error "default value required") :type term  :read-only t)
   (dst (error "default value required") :type term  :read-only t))


(defstruct (rewrite-rule-set
             (:print-object
               (lambda (object stream)
                 (format stream "{~{~A~^,~}}"
                         (rewrite-rule-set.rewrite-rules object))))
             (:constructor rewrite-rule-set (rewrite-rules))
             (:conc-name rewrite-rule-set.))
  (rewrite-rules nil :type %rewrite-rule-set :read-only t))


(defstruct (function-symbol-ordering
             (:print-object
              (lambda (object stream)
                (format stream "~{~A ~^< ~}"
                        (function-symbol-ordering.ordering object))))
             (:constructor function-symbol-ordering (ordering))
             (:conc-name function-symbol-ordering.))
  (ordering nil :type %function-symbol-ordering :read-only t))

(defstruct (mkbtt-form
             (:print-object
               (lambda (object stream)
                 (format stream "(VAR ~{~A~^ ~})~%" (mkbtt-form.var object))
                 (format stream "(RULES~%~{~A~%  ~})" 
                         (equation-set.equations
                           (mkbtt-form.rules object)))))
             (:constructor mkbtt-form (var rules))
             (:conc-name mkbtt-form.))
  (var   nil :type %mkbtt-var-type :read-only t)
  (rules nil :type equation-set :read-only t))
