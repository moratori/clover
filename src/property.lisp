(defpackage clover.property
  (:use :cl)
  (:export
    :*save-resolution-history*
    :*vterm-gensym-prefix*
    )
  )
(in-package :clover.property)


(defparameter *save-resolution-history* t
  "whether to save resolution history")

(defparameter *vterm-gensym-prefix* "V_"
  "prefix of generated symbol for rename process")

(defparameter *resolution-search-depth* 20
  "depth for search algorithms")
