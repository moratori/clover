(defpackage clover.tests.converter
  (:use :cl
        :clover.types
        :clover.util
        :clover.parser
        :clover.converter
        :1am)
  )
(in-package :clover.tests.converter)



(test clover.tests.converter.convert-to-equation-set.test1
      (let* ((data
               "(VAR)
               (RULES
               op(INIT,   APPLICATION)   -> APPLIED
               )")
             (parsed
               (parse-mkbtt-expression data)))
        (multiple-value-bind (consts functions)
            (clover.multicompletion::collect-symbol parsed)
          (is
            (and 
              (= (length functions) 1)
              (eq (first functions) 'CLOVER.PARSER::OP))))))
