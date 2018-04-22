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
  :license ""
  :depends-on (:iddfs)
  :components ((:module "src"
                :components
                ((:file "conditions")
                 (:file "types")
                 (:file "util")
                 (:file "clover"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "clover-test"))))
