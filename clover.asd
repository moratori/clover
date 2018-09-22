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
               #+sbcl :sb-cover)
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
                  ((:file "iddfs")))
                 (:file "property")
                 (:file "conditions")
                 (:file "types")
                 (:file "util")
                 (:file "substitute")
                 (:file "unify")
                 (:file "rename")
                 (:file "simplify")
                 (:file "resolution")
                 (:file "clover"))))
  :description "Simple Automated Theorem Prover"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md")))
