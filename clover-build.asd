#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#

#|
  Author: moratori
|#

(defsystem "clover-build"
  :version "2.5.0"
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
               :cl-custom-hash-table
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
                :components
                ((:module "search"
                  :components 
                  ((:file "common")
                   (:file "iddfs")
                   (:file "extractor")
                   (:file "astar")
                   (:file "dfs")))
                 (:file "property")
                 (:file "conditions")
                 (:file "types")
                 (:file "util")
                 (:file "substitute")
                 (:file "converter")
                 (:file "parser")
                 (:file "unify")
                 (:file "rename")
                 (:file "termorder")
                 (:file "simplify")
                 (:file "resolution")
                 (:file "rewrite")
                 (:file "criticalpair")
                 (:file "completion")
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
