(defpackage clover.parser
  (:use :cl
        :yacc
        :cl-lex
        :clover.property
        :clover.conditions
        :clover.types
        :clover.util
        )
  (:export 
    :%intern-symbol-to-specified-package
    :parse-equation-logical-expression 
    :parse-premise-logical-expression
    :parse-conseq-logical-expression))

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
                             (format nil "error occurred while parsing string: ~A" string)))))) 

(defun parse-conseq-logical-expression (string)
  (handler-case 
      (parse-with-lexer 
        (%conseq-expression-lexer string)
        %conseq-expression-parser)
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "error occurred while parsing string: ~A" string))))))

(defun parse-equation-logical-expression (string)
  (handler-case 
      (parse-with-lexer 
        (%equation-expression-lexer string)
        %equation-expression-parser)
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "error occurred while parsing string: ~A" string))))))

(define-string-lexer %premise-expression-lexer
  ("\\|"       (return (values :or     'or)))
  ("\\!"       (return (values :not    'not)))
  (","         (return (values :comma  'comma)))
  ("\\("       (return (values :lparen 'lparen)))
  ("\\)"       (return (values :rparen 'rparen)))
  ("[A-Z]+"    (return (values :constant $@)))
  ("[a-z0-9]+" (return (values :symbol $@))))

(define-string-lexer %conseq-expression-lexer
  ("\\&"       (return (values :and     'and)))
  ("\\!"       (return (values :not    'not)))
  (","         (return (values :comma  'comma)))
  ("\\("       (return (values :lparen 'lparen)))
  ("\\)"       (return (values :rparen 'rparen)))
  ("[A-Z]+"    (return (values :constant $@)))
  ("[a-z0-9]+" (return (values :symbol $@))))

(define-string-lexer %equation-expression-lexer
  ("="         (return (values :equality 'equality)))
  ("\\("       (return (values :lparen   'lparen)))
  ("\\)"       (return (values :rparen   'rparen)))
  (","         (return (values :comma    'comma)))
  ("[A-Z]+"    (return (values :constant $@)))
  ("[a-z0-9]+" (return (values :symbol   $@))))

(define-parser %equation-expression-parser
  (:start-symbol equation)
  (:terminals    (:equality
                  :lparen
                  :rparen
                  :comma
                  :constant
                  :symbol))

  (equation

    (:symbol :equality :symbol
     (lambda (sym1 equality sym2)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (vterm (%intern-symbol-to-specified-package
                        (string-upcase sym1)))
               (vterm (%intern-symbol-to-specified-package
                        (string-upcase sym2)))))))

    (:symbol :equality :constant
     (lambda (sym equality constant)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (vterm (%intern-symbol-to-specified-package
                        (string-upcase sym)))
               (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase constant))
                 nil)))))

    (:constant :equality :symbol
     (lambda (constant equality sym)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase constant))
                 nil)
               (vterm (%intern-symbol-to-specified-package
                        (string-upcase sym)))))))

    (:constant :equality :constant
     (lambda (constant1 equality constant2)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase constant1))
                 nil)
               (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase constant2))
                 nil)))))

    (:symbol argument :equality :symbol argument
     (lambda (sym1 argument1 equality sym2 argument2)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym1))
                 argument1)
               (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym2))
                 argument2)))))

    (:symbol argument :equality :symbol
     (lambda (sym1 argument1 equality sym2)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym1))
                 argument1)
               (vterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym2)))))))

    (:symbol :equality :symbol argument
     (lambda (sym1 equality sym2 argument)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list 
           (vterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym1)))
           (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym2)) 
                 argument)))))

    (:symbol argument :equality :constant
     (lambda (sym1 argument1 equality constant)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym1))
                 argument1)
               (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase constant))
                 nil)))))

    (:constant :equality :symbol argument 
     (lambda (constant equality sym argument)
       (literal
         nil
         (%intern-symbol-to-specified-package "=")
         (list 
           (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase constant))
                 nil)
           (fterm 
                 (%intern-symbol-to-specified-package
                   (string-upcase sym))
                 argument))))))

  (argument
    (:lparen termseq :rparen
     (lambda (lparen termseq rparen)
       (declare (ignore lparen rparen))
       termseq))) 

  (termseq

    (:constant
     (lambda (constant)
       (list 
         (fterm 
           (%intern-symbol-to-specified-package 
             (string-upcase constant))
           nil))))

    (:symbol
     (lambda (symbol)
       (list 
         (vterm 
           (%intern-symbol-to-specified-package 
             (string-upcase symbol))))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
       (list 
         (fterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           termseq))))

    (termseq :comma :constant
     (lambda (termseq comma constant)
       (declare (ignore comma))
       (append termseq 
               (list
                 (fterm 
                   (%intern-symbol-to-specified-package
                     (string-upcase constant))
                   nil)))))

    (termseq :comma :symbol
     (lambda (termseq comma symbol)
       (declare (ignore comma))
       (append termseq
               (list (vterm (%intern-symbol-to-specified-package
                              (string-upcase symbol)))))))

    (termseq :comma :symbol :lparen termseq :rparen
     (lambda (termseq1 comma symbol lparen termseq2 rparen)
       (declare (ignore comma lparen rparen))
       (append termseq1
               (list 
                 (fterm
                   (%intern-symbol-to-specified-package
                     (string-upcase symbol))
                   termseq2)))))))


(define-parser %premise-expression-parser 
  (:start-symbol premise)
  (:terminals    (:or
                  :not
                  :lparen
                  :rparen
                  :comma
                  :constant
                  :symbol))
  (premise

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

    (premise :or :symbol argument
     (lambda (premise or symbol argument)
       (declare (ignore or))
       (clause 
         (append 
           (clause.literals premise)
           (list 
             (literal
               nil
               (%intern-symbol-to-specified-package 
                 (string-upcase symbol))
               argument))))))

    (premise :or :not :symbol argument
     (lambda (premise or not symbol argument)
       (declare (ignore or not))
       (clause 
         (append 
           (clause.literals premise)
           (list 
             (literal
               t
               (%intern-symbol-to-specified-package
                 (string-upcase symbol) )
               argument)))))))

  (argument

    (:lparen :rparen
     (lambda (lparen rparen)
       (declare (ignore lparen rparen))
       nil))

    (:lparen termseq :rparen
     (lambda (lparen termseq rparen)
       (declare (ignore lparen rparen))
       termseq)))


  (termseq

    (:constant
     (lambda (constant)
       (list 
         (fterm 
           (%intern-symbol-to-specified-package 
             (string-upcase constant))
           nil))))

    (:symbol
     (lambda (symbol)
       (list 
         (vterm 
           (%intern-symbol-to-specified-package 
             (string-upcase symbol))))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
       (list 
         (fterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           termseq))))

    (termseq :comma :constant
     (lambda (termseq comma constant)
       (declare (ignore comma))
       (append termseq 
               (list
                 (fterm 
                   (%intern-symbol-to-specified-package
                     (string-upcase constant))
                   nil)))))

    (termseq :comma :symbol
     (lambda (termseq comma symbol)
       (declare (ignore comma))
       (append termseq
               (list (vterm (%intern-symbol-to-specified-package
                              (string-upcase symbol)))))))

    (termseq :comma :symbol :lparen termseq :rparen
     (lambda (termseq1 comma symbol lparen termseq2 rparen)
       (declare (ignore comma lparen rparen))
       (append termseq1
               (list 
                 (fterm
                   (%intern-symbol-to-specified-package
                     (string-upcase symbol))
                   termseq2)))))))


(define-parser %conseq-expression-parser 
  (:start-symbol conseq)
  (:terminals    (:and
                  :not
                  :lparen
                  :rparen
                  :comma
                  :constant
                  :symbol))
  (conseq

    (:symbol argument
     (lambda (symbol argument)
       (clause
         (list 
           (literal 
             t
             (%intern-symbol-to-specified-package 
               (string-upcase symbol))
             argument))
         nil nil nil :conseq
         )))

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
         nil nil nil :conseq
         )))

    (conseq :and :symbol argument
     (lambda (conseq and symbol argument)
       (declare (ignore and))
       (clause 
         (append 
           (clause.literals conseq)
           (list 
             (literal
               t
               (%intern-symbol-to-specified-package 
                 (string-upcase symbol))
               argument)))
         nil nil nil :conseq
         )))

    (conseq :and :not :symbol argument
     (lambda (conseq and not symbol argument)
       (declare (ignore and not))
       (clause 
         (append 
           (clause.literals conseq)
           (list 
             (literal
               nil
               (%intern-symbol-to-specified-package
                 (string-upcase symbol) )
               argument)))
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


  (termseq

    (:constant
     (lambda (constant)
       (list 
         (fterm 
           (%intern-symbol-to-specified-package 
             (string-upcase constant))
           nil))))

    (:symbol
     (lambda (symbol)
       (list 
         (vterm 
           (%intern-symbol-to-specified-package 
             (string-upcase symbol))))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
       (list 
         (fterm
           (%intern-symbol-to-specified-package
             (string-upcase symbol))
           termseq))))

    (termseq :comma :constant
     (lambda (termseq comma constant)
       (declare (ignore comma))
       (append termseq 
               (list
                 (fterm 
                   (%intern-symbol-to-specified-package
                     (string-upcase constant))
                   nil)))))

    (termseq :comma :symbol
     (lambda (termseq comma symbol)
       (declare (ignore comma))
       (append termseq
               (list (vterm (%intern-symbol-to-specified-package
                              (string-upcase symbol)))))))

    (termseq :comma :symbol :lparen termseq :rparen
     (lambda (termseq1 comma symbol lparen termseq2 rparen)
       (declare (ignore comma lparen rparen))
       (append termseq1
               (list 
                 (fterm
                   (%intern-symbol-to-specified-package
                     (string-upcase symbol))
                   termseq2)))))))
