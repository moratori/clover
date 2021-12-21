(defpackage clover.property
  (:use :cl)
  (:export
    :*save-resolution-history*
    :*vterm-gensym-prefix*
    :*resolution-search-depth*
    :*opener-algorithm*
    :*parsed-symbol-intern-package*
    :*vterm-for-human-readable*
    :*available-term-order-algorithms*
    :*term-order-algorithm*
    :*take-limit-from-permutation-generator*
    :*completion-giveup-threshold*
    )
  )
(in-package :clover.property)


(defparameter *save-resolution-history* nil
  "whether to save resolution history")

(defparameter *vterm-gensym-prefix* "v"
  "prefix of generated symbol for rename process")

(defparameter *resolution-search-depth* 40
  "depth for search algorithms")

(defparameter *completion-giveup-threshold* 20)

(defparameter *parsed-symbol-intern-package* "CLOVER.PARSER"
  "where to intern symbol ")

(defparameter *vterm-for-human-readable*
  (list "X" "Y" "Z" "W" "S" "T" "U" "V" "M" "N" "X0" "X1" "X2" "X3" "X4" "X5"))

(defparameter *term-order-algorithm* :lpo)

(defparameter *available-term-order-algorithms*
  (list :lpo))

(defparameter *take-limit-from-permutation-generator* 1024)
