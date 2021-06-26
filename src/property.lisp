(defpackage clover.property
  (:use :cl)
  (:export
    :*save-resolution-history*
    :*vterm-gensym-prefix*
    :*resolution-search-depth*
    :*opener-algorithm*
    :*parsed-symbol-intern-package*
    :*vterm-for-human-readable*
    )
  )
(in-package :clover.property)


(defparameter *save-resolution-history* nil
  "whether to save resolution history")

(defparameter *vterm-gensym-prefix* "v"
  "prefix of generated symbol for rename process")

(defparameter *resolution-search-depth* 40
  "depth for search algorithms")

(defparameter *parsed-symbol-intern-package* "CLOVER.PARSER"
  "where to intern symbol ")

(defparameter *vterm-for-human-readable*
  (list "X" "Y" "Z" "W" "S" "T" "U" "V" "M" "N"))
