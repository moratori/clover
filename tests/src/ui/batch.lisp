(defpackage clover.tests.ui.batch
  (:use :cl
        :1am))
(in-package :clover.tests.ui.batch)


(defparameter *trs-file* (namestring
                           (merge-pathnames 
                             #P"tests/test-output-files/sample.trs"
                             (asdf:system-source-directory :clover))))

(defparameter 
  *trs-file-data* 
  "(VAR x y)
(RULES
cons(S, cons(W, x)) -> cons(W, x)
cons(x, cons(S, cons(W, y))) -> cons(x, cons(W, y))
cons(W, cons(B, x)) -> cons(S, x)
cons(x, cons(W, cons(B, y))) -> cons(x, cons(S, y))
)")

(with-open-file (out *trs-file* :direction :output :if-exists :supersede)
  (format out "~A~%" *trs-file-data*))


(test clover.tests.ui.batch.%perform-command.test1

      (is 
        (clover.ui.batch::%perform-command
               :COMPLETE
               (list *trs-file*)))
      (is 
        (not 
          (clover.ui.batch::%perform-command
            :REWRITE
            (list *trs-file* "cons(W, cons(S, NIL))"))))

      )
