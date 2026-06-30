#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#

#|
  Author: moratori
|#

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cover)
  (require :sb-sprof))

(defsystem "clover"
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
               #+sbcl :sb-cover
               #+sbcl :sb-sprof
               )
  :components ((:module "src"
                :around-compile 
                 (lambda (thunk)
                   (declare (optimize
                              (debug 3)
                              (safety 3)
                              (speed 0)
                              (space 0)
                              (compilation-speed 0)))
                   ;; CLOVER_DISABLE_COVERAGE が設定されている場合は計装を行わない。
                   ;; 計装済み fasl は実行が大幅に遅くなるため、非力な CI 環境では
                   ;; これを外してテスト全体の実行時間を短縮する。
                   #+sbcl
                   (unless (uiop:getenvp "CLOVER_DISABLE_COVERAGE")
                     (declaim (optimize
                                (sb-cover:store-coverage-data 3))))
                   (funcall thunk))
                ;; ソースファイル一覧は clover-build.asd と共通のため src-components.lisp に括り出している。
                :components
                #.(read-file-form
                   (subpathname *load-pathname* "src-components.lisp"))))
  :description "Simple Automated Theorem Prover"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md")))
