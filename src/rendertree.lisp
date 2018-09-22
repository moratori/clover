(defpackage clover.rendertree
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        )
  (:export
    )
  )
(in-package :clover.rendertree)


(defmethod render-refutation-tree ((clause clause) (filepath pathname))
  )
