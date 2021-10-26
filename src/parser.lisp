(defpackage clover.parser
  (:use :cl
        :yacc
        :cl-lex
        :clover.property
        :clover.conditions
        :clover.types
        :clover.util
        :clover.converter
        )
  (:export 
    :%intern-symbol-to-specified-package
    :parse-premise-logical-expression
    :parse-conseq-logical-expression
    :parse-mkbtt-expression
    ))

(in-package :clover.parser)


(defun %intern-symbol-to-specified-package (string)
  (intern string *parsed-symbol-intern-package*))

(defun parse-premise-logical-expression (string)
  (handler-case
      (parse-with-lexer 
        (%premise-expression-lexer string)
        %premise-expression-parser)
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "~A error occurred while parsing string: ~A" con string)))))) 

(defun parse-conseq-logical-expression (string)
  (handler-case 
      (parse-with-lexer 
        (%conseq-expression-lexer string)
        %conseq-expression-parser)
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "~A error occurred while parsing string: ~A" con string))))))

(defun parse-mkbtt-expression (string)
  (handler-case 
      (convert-to-equation-set
        (parse-with-lexer 
          (%mkbtt-expression-lexer string)
          %mkbtt-form-parser))
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "~A error occurred while parsing string: ~A" con string))))))


(define-string-lexer %premise-expression-lexer
  ("="         (return (values :equality 'equality)))
  ("\\|"       (return (values :or     'or)))
  ("\\!"       (return (values :not    'not)))
  (","         (return (values :comma  'comma)))
  ("\\("       (return (values :lparen 'lparen)))
  ("\\)"       (return (values :rparen 'rparen)))
  ("\\["       (return (values :list-lparen 'list-lparen)))
  ("\\]"       (return (values :list-rparen 'list-rparen)))
  ("[A-Z]+"    (return (values :constant $@)))
  ("[a-z0-9]+" (return (values :symbol $@))))

(define-string-lexer %conseq-expression-lexer
  ("="         (return (values :equality 'equality)))
  ("\\&"       (return (values :and     'and)))
  ("\\!"       (return (values :not    'not)))
  (","         (return (values :comma  'comma)))
  ("\\("       (return (values :lparen 'lparen)))
  ("\\)"       (return (values :rparen 'rparen)))
  ("\\["       (return (values :list-lparen 'list-lparen)))
  ("\\]"       (return (values :list-rparen 'list-rparen)))
  ("[A-Z]+"    (return (values :constant $@)))
  ("[a-z0-9]+" (return (values :symbol $@))))


(define-string-lexer %mkbtt-expression-lexer
  ("RULES"     (return (values :rules 'rules)))
  ("VAR"       (return (values :var   'var)))
  ("->"        (return (values :equality 'equality)))
  (","          (return (values :comma  'comma)))
  ("\\("        (return (values :lparen 'lparen)))
  ("\\)"        (return (values :rparen 'rparen)))
  ("[a-zA-Z0-9_]+" (return (values :symbol $@))))


(define-parser %premise-expression-parser 
  (:start-symbol premise)
  (:terminals    (:or
                  :not
                  :lparen
                  :rparen
                  :list-lparen
                  :list-rparen
                  :comma
                  :equality
                  :constant
                  :symbol))
  (premise

    (term :equality term
     (lambda (term1 equality term2)
       (clause (list (equation nil term1 term2)))))

    (term :not :equality term
     (lambda (term1 neg equality term2)
       (clause (list (equation t term1 term2)))))

    (:symbol argument
     (lambda (symbol argument)
       (clause
         (list 
           (literal 
             nil
             (%intern-symbol-to-specified-package 
               (string-upcase symbol))
             argument)))))

    (:not :symbol argument
     (lambda (not symbol argument)
       (declare (ignore not))
       (clause
         (list
           (literal
             t
             (%intern-symbol-to-specified-package 
               (string-upcase symbol))
             argument)))))

    (premise :or premise
     (lambda (premise-left or premise-right)
       (declare (ignore or))
       (clause 
         (append 
           (clause.literals premise-left)
           (clause.literals premise-right)
           )))))

  (argument

    (:lparen :rparen
     (lambda (lparen rparen)
       (declare (ignore lparen rparen))
       nil))

    (:lparen termseq :rparen
     (lambda (lparen termseq rparen)
       (declare (ignore lparen rparen))
       termseq)))

  (term

    (:list-lparen :list-rparen
     (lambda (lparen rparen)
       (constant
         (%intern-symbol-to-specified-package "NIL"))))

    (:list-lparen termseq :list-rparen
     (lambda (lparen termseq rparen)
       (let ((nl (constant (%intern-symbol-to-specified-package "NIL")))
             (fname (%intern-symbol-to-specified-package "CONS")))
         (labels
             ((inner (arg)
                (if (null arg)
                    nl
                    (fterm fname (list (car arg) (inner (cdr arg)))))))
           (inner termseq)))))

    (:constant
     (lambda (constant)
         (constant
           (%intern-symbol-to-specified-package 
             (string-upcase constant)))))

    (:symbol
     (lambda (symbol)
         (vterm 
           (%intern-symbol-to-specified-package 
             (string-upcase symbol)))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
         (fterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           termseq))))

  (termseq

    (term
      (lambda (term)
        (list term)))

    (termseq :comma term
     (lambda (termseq comma term)
       (declare (ignore comma))
       (append termseq
               (list term))))))


(define-parser %conseq-expression-parser 
  (:start-symbol conseq)
  (:terminals    (:and
                  :not
                  :lparen
                  :rparen
                  :list-lparen
                  :list-rparen
                  :comma
                  :equality
                  :constant
                  :symbol))
  (conseq

    (term :equality term
     (lambda (term1 equality term2)
       (clause (list (equation t term1 term2)))))

    (term :not :equality term
     (lambda (term1 neg equality term2)
       (clause (list (equation nil term1 term2)))))

    (:symbol argument
     (lambda (symbol argument)
       (clause
         (list 
           (literal 
             t
             (%intern-symbol-to-specified-package 
               (string-upcase symbol))
             argument))
         nil nil nil :conseq)))

    (:not :symbol argument
     (lambda (not symbol argument)
       (declare (ignore not))
       (clause
         (list
           (literal
             nil
             (%intern-symbol-to-specified-package 
               (string-upcase symbol))
             argument))
         nil nil nil :conseq)))

    (conseq :and conseq
     (lambda (conseq-left and conseq-right)
       (declare (ignore and))
       (clause 
         (append 
           (clause.literals conseq-left)
           (clause.literals conseq-right))
         nil nil nil :conseq
         ))))

  (argument

    (:lparen :rparen
     (lambda (lparen rparen)
       (declare (ignore lparen rparen))
       nil))

    (:lparen termseq :rparen
     (lambda (lparen termseq rparen)
       (declare (ignore lparen rparen))
       termseq)))

  (term

    (:list-lparen :list-rparen
     (lambda (lparen rparen)
       (constant
         (%intern-symbol-to-specified-package "NIL"))))

    (:list-lparen termseq :list-rparen
     (lambda (lparen termseq rparen)
       (let ((nl (constant (%intern-symbol-to-specified-package "NIL")))
             (fname (%intern-symbol-to-specified-package "CONS")))
         (labels
             ((inner (arg)
                (if (null arg)
                    nl
                    (fterm fname (list (car arg) (inner (cdr arg)))))))
           (inner termseq)))))

    (:constant
     (lambda (constant)
         (constant
           (%intern-symbol-to-specified-package 
             (string-upcase constant)))))

    (:symbol
     (lambda (symbol)
         (vterm 
           (%intern-symbol-to-specified-package 
             (string-upcase symbol)))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
         (fterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           termseq))))

  (termseq

    (term
      (lambda (term)
        (list term)))

    (termseq :comma term
     (lambda (termseq comma term)
       (declare (ignore comma))
       (append termseq
               (list term))))))


(define-parser %mkbtt-form-parser
  (:start-symbol mkbtt)
  (:terminals    (:rules
                  :var
                  :equality
                  :comma
                  :lparen
                  :rparen
                  :symbol))

  (mkbtt
    (:lparen :var var-list :rparen :lparen :rules equation-list :rparen
     (lambda (lparen_1_ var_   var-list rparen_1_
              lparen_2_ rules_ equation-list rparen_2_)
       (mkbtt-form var-list equation-list)))

    (:lparen :var :rparen :lparen :rules equation-list :rparen
     (lambda (lparen_1_ var_   rparen_1_
              lparen_2_ rules_ equation-list rparen_2_)
       (mkbtt-form nil equation-list))))

  (var-list
    (:symbol
     (lambda (symbol)
       (list 
         (vterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           symbol
           ))) )
    (:symbol var-list
     (lambda (symbol vl)
       (cons 
         (vterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           symbol
           )
         vl))))

  (equation-list
    (term :equality term
     (lambda (term1 _ term2)
       (equation-set
         (list
           (equation nil term1 term2)))))
    (term :equality term equation-list
     (lambda (term1 _ term2 eqlist)
       (equation-set
         (cons
           (equation nil term1 term2)
           (equation-set.equations eqlist))))))
  (term
    (:symbol
     (lambda (symbol)
       (vterm
         (%intern-symbol-to-specified-package
           (string-upcase symbol))
         symbol
         )))
    (:symbol :lparen :rparen
     (lambda (symbol lparen rparen)
       (declare (ignore lparen rparen))
       (constant
         (%intern-symbol-to-specified-package
           (string-upcase symbol)))))
    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
         (fterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           termseq
           ))))
  (termseq
    (term
      (lambda (term)
        (list term)))
    (termseq :comma term
     (lambda (termseq comma term)
       (declare (ignore comma))
       (append termseq
               (list term)))))
  )

