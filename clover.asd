#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#

#|
  Author: moratori
|#

(defsystem "clover"
  :version "0.1.0"
  :author "moratori"
  :license "LLGPL"
  :depends-on (:cl-cpus
               :bordeaux-threads
               :yacc
               :cl-lex
               :alexandria
               :cl-ppcre
               :cl-custom-hash-table
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
                   (:file "extractor")
                   (:file "dfs")))
                 (:file "property")
                 (:file "conditions")
                 (:file "types")
                 (:file "util")
                 (:file "parser")
                 (:file "termorder")
                 (:file "substitute")
                 (:file "unify")
                 (:file "rename")
                 (:file "simplify")
                 (:file "resolution")
                 (:file "rewrite")
                 (:file "criticalpair")
                 (:file "completion")
                 (:file "clover")
                 (:file "rendertree")
                 (:module "repl"
                  :components 
                  ((:file "util")
                   (:file "repl"))))))
  :description "Simple Automated Theorem Prover"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md")))
