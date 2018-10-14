(defpackage clover.property
  (:use :cl)
  (:export
    :*save-resolution-history*
    :*vterm-gensym-prefix*
    :*resolution-search-depth*
    :*resolution-algorithm*
    :*parsed-symbol-intern-package*
    )
  )
(in-package :clover.property)


(defparameter *save-resolution-history* t
  "whether to save resolution history")

(defparameter *vterm-gensym-prefix* "V_"
  "prefix of generated symbol for rename process")

(defparameter *resolution-search-depth* 20
  "depth for search algorithms")

(defparameter *resolution-algorithm* :exhaustive
  "default resolution algorithm")

(defparameter *parsed-symbol-intern-package* "CLOVER.PARSER"
  "where to intern symbol ")
