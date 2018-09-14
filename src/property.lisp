(defpackage clover.property
  (:use :cl)
  (:export
    :*save-resolution-history*
    )
  )
(in-package :clover.property)


(defparameter *save-resolution-history* nil
  "whether to save resolution history")
