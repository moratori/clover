(defpackage clover.property
  (:use :cl)
  (:export
    :*save-resolution-history*
    :*vterm-gensym-prefix*
    :*resolution-search-depth*
    :*resolution-algorithm*
    :*supported-resolution-algorithms*
    :*parsed-symbol-intern-package*
    )
  )
(in-package :clover.property)


(defparameter *save-resolution-history* nil
  "whether to save resolution history")

(defparameter *vterm-gensym-prefix* "v"
  "prefix of generated symbol for rename process")

(defparameter *resolution-search-depth* 20
  "depth for search algorithms")

(defparameter *resolution-algorithm* :exhaustive
  "default resolution algorithm.
   one of *supported-resolution-algorithms* must be selected here")

(defparameter *supported-resolution-algorithms*
  '(:exhaustive :linear :horn)
  "here algorithms must be implemented in clover.resolution package")

(defparameter *parsed-symbol-intern-package* "CLOVER.PARSER"
  "where to intern symbol ")

