(defpackage clover.parser
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types
        :clover.util
        :clover.converter
        )
  (:import-from :yacc
                :define-parser
                :parse-with-lexer
                )
  (:import-from :lexer
                :define-lexer
                :with-lexer
                :with-token-reader
                :push-lexer
                :pop-lexer
                )
  (:export 
    :%intern-symbol-to-specified-package
    :parse-premise-logical-expression
    :parse-conseq-logical-expression
    :parse-mkbtt-expression
    ))

(in-package :clover.parser)


(defun %intern-symbol-to-specified-package (string)
  (intern 
    (string-upcase string)
    *parsed-symbol-intern-package*))

(defun parse-premise-logical-expression (string)
  (handler-case
      (with-lexer (lexer 'premise-expression-lexer string)
        (with-token-reader (token-reader lexer)
          (parse-with-lexer token-reader %premise-expression-parser)))
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "~A error occurred while parsing string: ~A" con string)))))) 

(defun parse-conseq-logical-expression (string)
  (handler-case 
      (with-lexer (lexer 'conseq-expression-lexer string)
        (with-token-reader (token-reader lexer)
          (parse-with-lexer token-reader %conseq-expression-parser)))

    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "~A error occurred while parsing string: ~A" con string))))))

(defun parse-mkbtt-expression (string)
  (handler-case 
      (with-lexer (lexer 'mkbtt-toplevel-lexer string)
        (with-token-reader (token-reader lexer)
          (convert-to-equation-set
            (mkbtt-form 
              (parse-with-lexer token-reader %mkbtt-form-parser)))))
    (condition (con)
      (error (make-condition 'expr-parse-error 
                             :message 
                             (format nil "~A error occurred while parsing string: ~A" con string))))))

(define-lexer premise-expression-lexer (state)
  ("[%s%n]+"      :next-token)
  ("="         (values :equality 'equality))
  ("%|"        (values :or     'or))
  ("%!"         (values :not    'not))
  (","         (values :comma  'comma))
  ("%("        (values :lparen 'lparen))
  ("%)"        (values :rparen 'rparen))
  ("%["        (values :list-lparen 'list-lparen))
  ("%]"        (values :list-rparen 'list-rparen))
  ("[A-Z]+"    (values :constant $$))
  ("[a-z0-9_%+%*%.%-/@\\]+" (values :symbol $$)))

(define-lexer conseq-expression-lexer (state)
  ("[%s%n]+"      :next-token)
  ("="         (values :equality 'equality))
  ("&"         (values :and     'and))
  ("%!"         (values :not    'not))
  (","         (values :comma  'comma))
  ("%("        (values :lparen 'lparen))
  ("%)"        (values :rparen 'rparen))
  ("%["        (values :list-lparen 'list-lparen))
  ("%]"        (values :list-rparen 'list-rparen))
  ("[A-Z]+"    (values :constant $$))
  ("[a-z0-9_%+%*%.%-/@\\]+" (values :symbol $$)))

(define-lexer mkbtt-comment-lexer (state)
  ("[^%(%)]+"     (values :simple-comment-content $$))
  ("%("           (push-lexer state #'mkbtt-comment-lexer :lparen)) 
  ("%)"           (pop-lexer state :rparen)))

(define-lexer mkbtt-var-lexer (state)
  ("[%s%n]+"               :next-token)
  ("[a-zA-Z0-9_]+" (values :var-symbol $$))
  ("%)"                    (pop-lexer state :rparen)))

(define-lexer mkbtt-rules-lexer (state)
  ("[%s%n]+"           :next-token)
  ("%->"               (values :equality 'equality))
  (","                 (values :comma  'comma))
  ("[a-zA-Z0-9_%+%*%.%-/@&\\]+" (values :rules-symbol $$))
  ("%("                (push-lexer state #'mkbtt-rules-lexer :lparen) )
  ("%)"                (pop-lexer state :rparen)))

(define-lexer mkbtt-toplevel-lexer (state)
  ("[%s%n]+"      :next-token)
  ("%(COMMENT"    (push-lexer state #'mkbtt-comment-lexer :start-comment))
  ("%(VAR"        (push-lexer state #'mkbtt-var-lexer     :start-var))
  ("%(RULES"      (push-lexer state #'mkbtt-rules-lexer   :start-rules))
  ("%(FROM|%(from"(push-lexer state #'mkbtt-comment-lexer :start-from))
  )



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
             (%intern-symbol-to-specified-package symbol)
             argument)))))

    (:not :symbol argument
     (lambda (not symbol argument)
       (declare (ignore not))
       (clause
         (list
           (literal
             t
             (%intern-symbol-to-specified-package symbol)
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
           (%intern-symbol-to-specified-package constant))))

    (:symbol
     (lambda (symbol)
         (vterm 
           (%intern-symbol-to-specified-package symbol))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
         (fterm
           (%intern-symbol-to-specified-package symbol)
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
             (%intern-symbol-to-specified-package symbol)
             argument))
         nil nil nil :conseq)))

    (:not :symbol argument
     (lambda (not symbol argument)
       (declare (ignore not))
       (clause
         (list
           (literal
             nil
             (%intern-symbol-to-specified-package symbol)
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
           (%intern-symbol-to-specified-package constant))))

    (:symbol
     (lambda (symbol)
         (vterm 
           (%intern-symbol-to-specified-package symbol))))

    (:symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
         (fterm
           (%intern-symbol-to-specified-package symbol)
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
  (:start-symbol mkbtt-form)
  (:terminals    (:lparen
                  :rparen
                  :comma
                  :equality
                  :start-from
                  :start-comment
                  :start-var
                  :start-rules
                  :var-symbol
                  :rules-symbol
                  :simple-comment-content))
  (mkbtt-form
    (statement
      (lambda (statement) (list statement)))
    (mkbtt-form statement
      (lambda (mkbtt-form statement)
        (append mkbtt-form (list statement)))))

  (statement
    (:start-comment comment-content :rparen
     (lambda (start-comment comment-content rparen)
       (declare (ignore start-comment rparen))
       (mkbtt-comment-form
         comment-content)))
    (:start-from comment-content :rparen
     (lambda (start-from comment-content rparen)
       (declare (ignore start-from rparen))
       (mkbtt-comment-form
         comment-content)))
    (:start-var :rparen
     (lambda (start-var rparen)
       (declare (ignore start-var rparen))
       (mkbtt-var-form nil)))
    (:start-var var-list :rparen
     (lambda (start-var var-list rparen)
       (declare (ignore start-var rparen))
       (mkbtt-var-form
         var-list)))
    (:start-rules equations :rparen
     (lambda (start-rules equations rparen)
       (mkbtt-rules-form
         (equation-set equations)))))

  (equations
    (equation
      (lambda (equation) (list equation)))
    (equations equation
      (lambda (equations equation)
        (append equations (list equation)))))

  (equation
    (term :equality term
      (lambda (left equality right)
        (declare (ignore equality))
        (equation nil left right))))

  (term
    (:rules-symbol
     (lambda (symbol)
       (vterm
         (%intern-symbol-to-specified-package symbol)
         symbol)))
    (:rules-symbol :lparen :rparen
     (lambda (symbol lparen rparen)
       (declare (ignore lparen rparen))
       ;; constant
       (constant
         (%intern-symbol-to-specified-package symbol))))
    (:rules-symbol :lparen termseq :rparen
     (lambda (symbol lparen termseq rparen)
       (declare (ignore lparen rparen))
       (fterm (%intern-symbol-to-specified-package symbol) termseq))))

  (termseq
    (term
      (lambda (term)
        (list term)))
    (termseq :comma term
     (lambda (termseq comma term)
       (declare (ignore comma))
       (append termseq
               (list term)))))

  (var-list
    (:var-symbol
     (lambda (symbol)
       (list (vterm 
               (%intern-symbol-to-specified-package symbol)
               symbol))))
    (var-list :var-symbol
     (lambda (var-list symbol)
       (append 
         var-list
         (list (vterm (%intern-symbol-to-specified-package symbol)
                      symbol))))))

  (comment-content
    (:simple-comment-content
     (lambda (x) x))
    (:lparen :rparen
     (lambda (x y)
       (declare (ignore x y))
       "()"))
    (:lparen :rparen comment-content
     (lambda (x y z)
       (declare (ignore x y))
       (concatenate 'string "()" z)))
    (:lparen comment-content :rparen
     (lambda (x y z) 
       (declare (ignore x z))
       (concatenate 'string "(" y ")")))
    (:lparen comment-content :rparen comment-content
     (lambda (x y z w) 
       (declare (ignore x z))
       (concatenate 'string "(" y ")" w)))
    (comment-content :lparen :rparen
     (lambda (x y z) 
       (declare (ignore y z))
       (concatenate 'string x "()")))
    (comment-content :lparen :rparen comment-content
     (lambda (x y z w)
       (declare (ignore y z))
       (concatenate 'string x "()" w)))
    (comment-content :lparen comment-content :rparen
     (lambda (x y z w) 
       (declare (ignore y w))
       (concatenate 'string x z)))
    (comment-content :lparen comment-content :rparen comment-content
     (lambda (x y z w v)
       (declare (ignore y w))
       (concatenate 'string x "(" z ")" v)))))
