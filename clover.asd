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
  :depends-on ("cl-cpus" "bordeaux-threads")
  :components ((:module "src"
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
