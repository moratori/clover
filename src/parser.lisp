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
