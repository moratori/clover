#|
  This file is a part of clover project.
  Copyright (c) 2018 moratori
|#


#|
  To execute clover test,
  1. load clover-test      : (ql:quickload :clover-test)
  2. in-package clover-test: (in-package :clover-test)
  3. run tests             : (run)
|#

(defsystem "clover-test"
  :author "moratori"
  :license ""
  :depends-on (:clover
               :1am
               #+sbcl :sb-cover
               )
  :components ((:module "tests"
                :components
                ((:module "src"
                  :components 
                  ((:module "search"
                    :components 
                    ((:file "iddfs")
                     (:file "extractor")
                     (:file "dfs")))
                   (:module "ui"
                    :components 
                    ((:file "batch")))
                   (:file "converter")
                   (:file "clover")
                   (:file "types")
                   (:file "unify")
                   (:file "util")
                   (:file "termorder")
                   (:file "rename")
                   (:file "simplify")
                   (:file "substitute")
                   (:file "resolution")
                   (:file "rewrite")
                   (:file "criticalpair")
                   (:file "completion")
                   (:file "multicompletion")
                   (:file "rendertree")
                   (:file "parser"))))))
  :description "Test system for clover")
