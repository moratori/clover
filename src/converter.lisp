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
  (let* ((var (mkbtt-form.var mkbtt-form))
         (eqs (equation-set.equations
                (mkbtt-form.rules mkbtt-form)))
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
    (equation-set ret)))
