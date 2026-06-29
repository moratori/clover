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
  :version "2.6.2"
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
                   #+sbcl
                   (declaim (optimize 
                              (sb-cover:store-coverage-data 3)))
                   (funcall thunk))
                :components
                ((:module "search"
                  :components 
                  ((:file "common")
                   (:file "iddfs")
                   (:file "astar")
                   (:file "dfs")))
                 (:file "property")
                 (:file "conditions")
                 (:file "types")
                 (:file "util")
                 (:file "substitute")
                 (:file "converter")
                 (:file "parser")
                 (:file "rename")
                 (:file "unify")
                 (:file "termorder")
                 (:file "simplify")
                 (:file "resolution")
                 (:file "rewrite")
                 (:file "criticalpair")
                 (:file "completion")
                 (:file "multiprocess")
                 (:file "multicompletion")
                 (:file "clover")
                 (:file "rendertree")
                 (:module "ui"
                  :components 
                  ((:file "util")
                   (:file "batch")
                   (:file "repl")
                   (:file "main"))))))
  :description "Simple Automated Theorem Prover"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md")))
