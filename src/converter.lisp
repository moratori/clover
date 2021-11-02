(defpackage clover.converter
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types
        :clover.substitute
        :clover.util
        )
  (:export 
    :convert-to-equation-set
    ))

(in-package :clover.converter)

(defmethod convert-to-equation-set ((clause-set clause-set))
  (let ((tmp
          (loop
            :named exit
            :for clause :in (clause-set.clauses clause-set)
            :for literals := (clause.literals clause)
            :for len := (length literals)
            :collect
            (if (or (< 1 len)
                    (zerop len)
                    (not (typep (first literals) 'equation))
                    (equation.negation (car literals)))
                (return-from exit nil)
                (first literals)))))
    (when tmp
      (equation-set tmp))))

(defmethod convert-to-equation-set ((mkbtt-form mkbtt-form))
  (let* ((forms (mkbtt-form.value mkbtt-form))
         (var-forms 
           (remove-if-not
             (lambda (x) (typep x 'mkbtt-var-form)) forms))
         (rules-forms
           (remove-if-not
             (lambda (x) (typep x 'mkbtt-rules-form)) forms)))

    (unless (and (= (length var-forms) 1)
                 (= (length rules-forms) 1))
      (error (make-condition
               'mkbtt-parse-error
               :message "invalid mkbtt form")))

    (let* ((var (mkbtt-var-form.value (first var-forms)))
           (eqs (equation-set.equations
                  (mkbtt-rules-form.value (first rules-forms))))
           (var-in-expr 
             (remove-duplicates
               (mapcan #'collect-variables eqs)
               :test 
               (lambda (x y)
                 (and 
                   (term= x y)
                   (string= (vterm.original-str x)
                            (vterm.original-str y))))))
           (should-be-constant
             (remove-if
               (lambda (v) 
                 (member v var
                         :test
                         (lambda (x y)
                           (and (term= x y)
                                (string= (vterm.original-str x)
                                         (vterm.original-str y))))))
               var-in-expr))
           (binding
             (unifier-set
               (mapcar
                 (lambda (x)
                   (unifier x (constant (vterm.var x))))
               should-be-constant)))
           (ret
             (mapcar
               (lambda (e)
                 (apply-unifier-set e binding))
               eqs)))
      (equation-set ret))))
