(defpackage clover.ui.batch
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types
        :clover.parser
        :clover.util
        :clover.clover
        :clover.converter
        :clover.ui.util
        :ppcre
        )
  (:import-from :clover.multicompletion
                :multi-kb-completion)
  (:import-from :clover.rename
                :rename-for-human-readable-printing)
  (:export 
    :main
    ))
(in-package :clover.ui.batch)



(defun help ()
  (%stdout
    "Command help
     complete <file name>       execute completion process~%")
  )



(defmethod %perform-command :around ((command (eql :COMPLETE)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (probe-file (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))


(defmethod %perform-command ((command (eql :COMPLETE)) args)
  (let* ((fname (first args))
         (content (alexandria:read-file-into-string fname))
         (eqs (parse-mkbtt-expression content))
         (completed (multi-kb-completion eqs 15)))
    (when completed
      (loop
        :for rule :in (rewrite-rule-set.rewrite-rules
                        (rename-for-human-readable-printing completed))
        :do (%stdout "~A~%" rule)))))


(defun main (args)
  (let ((subcommand (first args))
        (subcommand-args (cdr args)))
    (if (or (not (scan "[a-zA-Z]+" subcommand))
            (> (length subcommand) 9))
        (progn
          (%stdout "malformed command: ~A~%" subcommand)
          (help))
        (handler-case
            (let* ((command-name
                     (intern (string-upcase subcommand)
                             (find-package "KEYWORD")))
                   (available-command
                    (compute-applicable-methods
                      #'%perform-command
                      (list command-name subcommand-args))))
              (if available-command
                  (%perform-command command-name subcommand-args)
                  (%stdout "unimplemented command: ~A~%" subcommand)))
          (condition (con)
            (%stdout "unhandled condition occurred: ~A~%quit~%" con))))))

