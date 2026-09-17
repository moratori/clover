(defpackage clover.parameters
  (:use :cl)
  (:export
    :*save-resolution-history*
    :*vterm-gensym-prefix*
    :*opener-algorithm*
    :*parsed-symbol-intern-package*
    :*vterm-for-human-readable*
    :*available-term-order-algorithms*
    :*term-order-algorithm*
    :*take-limit-from-permutation-generator*
    :*completion-giveup-threshold*
    :*heuristic-weight*
    :*clause-permissiveness-weight*
    )
  )
(in-package :clover.parameters)


(defparameter *save-resolution-history* nil
  "whether to save resolution history")

(defparameter *vterm-gensym-prefix* "v"
  "prefix of generated symbol for rename process")

(defparameter *completion-giveup-threshold* 15)

(defparameter *parsed-symbol-intern-package* "CLOVER.PARSER"
  "where to intern symbol ")

(defparameter *vterm-for-human-readable*
  (list "X" "Y" "Z" "W" "S" "T" "U" "V" "M" "N" "X0" "X1" "X2" "X3" "X4" "X5"))

(defparameter *term-order-algorithm* :lpo)

(defparameter *available-term-order-algorithms*
  (list :lpo))

(defparameter *take-limit-from-permutation-generator* 1024)

(defparameter *heuristic-weight* 1.5
  "貪欲度。1.0 で最短性寄り、大きいほど速いが最短でなくなる。")

(defparameter *clause-permissiveness-weight* 10
  "cost-to-neighbor のスカスカ度因子の強度 k (因子 = 1 + k * mean(スカスカ度))。
   0 で因子が恒等になり従来式と完全に一致する。
   simple12+difficult3 コーパスでは k=1〜15 の全域で問題単位の退行なしに
   単調改善 (展開ノード数 285 → 266(k=2〜5) → 263(k=8) → 245(k=15))。
   大きいほど「スカスカな状態を避ける」方向に貪欲になるため、
   引き上げは広い問題群での非劣化確認を経て行うこと。")
