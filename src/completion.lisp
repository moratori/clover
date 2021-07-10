(defpackage clover.completion
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.rename
        :clover.rewrite
        :clover.termorder
        )
  (:export
    :kb-completion
    )
  )
(in-package :clover.completion)



(defmethod kb-completion ((equation-set equation-set))
  )
