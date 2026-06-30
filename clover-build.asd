#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#

#|
  Author: moratori
|#

(defsystem "clover-build"
  :version "2.6.4"
  :author "moratori"
  :license "LLGPL"
  :depends-on (:cl-cpus
               :cl-heap
               :lparallel
               :yacc
               :lexer ;https://github.com/massung/lexer.git
               :generators
               :alexandria
               :cl-ppcre
               )
  :components ((:module "src"
                :around-compile 
                 (lambda (thunk)
                   #+sbcl(declaim (sb-ext:muffle-conditions cl:warning))
                   (declaim (optimize
                              (debug 0)
                              (safety 0)
                              (speed 3)
                              (space 0)
                              (compilation-speed 0)))
                   (funcall thunk))
                ;; ソースファイル一覧は clover.asd と共通のため src-components.lisp に括り出している。
                :components
                #.(read-file-form
                   (subpathname *load-pathname* "src-components.lisp"))))
  :description "Simple Automated Theorem Prover"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md")))
