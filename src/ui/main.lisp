(defpackage clover.ui.main
  (:use :cl)
  (:export 
    :main))
(in-package :clover.ui.main)


(defun main (args)
  (if (> (length args) 0)
      (clover.ui.batch:main args)
      (clover.ui.repl:main)))
