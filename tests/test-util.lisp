(defpackage clover.tests.util 
  (:use :cl :1am) 
  (:export :skip-test))
(in-package :clover.tests.util)


(defmacro skip-test (test-form)
  "Wrap a (test NAME ...) form to skip it. The real test body is NOT emitted,
   so its assertions never run (safe even for non-terminating / WIP tests).
   A stub test is registered under the same NAME that only prints a skip
   notice at run time, keeping the skipped test visible in the suite output."
  (let ((name (second test-form)))
    `(test ,name
       (format t "~&[SKIPPED] ~A is skipped!! Please re-enable it at the appropriate time.~%"
               ,(string-downcase (symbol-name name))))))
