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
  (:import-from :clover.parser
                :parse-premise-logical-expression)
  (:import-from :clover.rewrite
                :rewrite-final)
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
     complete <file name>                        execute completion process
     rewrite  <file name> <term-1> ... <term-n>  execute completion and get irreducible term
                                                 lowercase alphabet appeared in term is treated as a variable
                                                 uppercase alphabet is treated as a constant~%")
  )



(defmethod %perform-command :around ((command (eql :COMPLETE)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (probe-file (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))

(defmethod %perform-command :around ((command (eql :REWRITE)) args)
  (cond 
    ((and 
          (<= 2 (length args))
          (probe-file (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))


(defmethod %perform-command ((command (eql :HELP)) args)
  (help))

(defun show-batch-completion-result (flag ordering result)
  (when flag
    (let* ((rules 
             (rewrite-rule-set.rewrite-rules
               (rename-for-human-readable-printing result)))
           (variables                 
             (remove-duplicates
               (mapcan
                 (lambda (x)
                   (append
                     (collect-variables
                       (rewrite-rule.src x))
                     (collect-variables
                       (rewrite-rule.dst x))))
                 rules)
               :test #'term=)))
      (%stdout "YES~%~%")
      (%stdout "(VAR ~{~A~^ ~})~%" variables)
      (%stdout "(RULES~%~{ ~A~%~})~%" rules)
      (%stdout "(COMMENT~% ~A~%)~%" ordering)))
  (unless flag
    (%stdout "MAYBE~%")))


(defmethod %perform-command ((command (eql :REWRITE)) args)
  (let* ((fname (first args))
         (terms (cdr args))
         (result (%perform-command :COMPLETE (list fname))))
    (when result
      (let ((irreducible
              (mapcar
                (lambda (term)
                  (let* ((parsed
                           (parse-premise-logical-expression
                             (format nil "~A = DUMMY" term)))
                         ;; 項単体でのパースができない為、
                         ;; ダミーの等式をパースしてから取り出す
                         (parsed-term
                           (equation.left 
                             (first (clause.literals parsed)))))
                    (list term (rewrite-final parsed-term result))))
                terms)))
        (%stdout "(COMMENT~%~:{ ~A reduced ~A~%~})~%" irreducible)))))


(defmethod %perform-command ((command (eql :COMPLETE)) args)
  (let* ((fname (first args))
         (content (alexandria:read-file-into-string fname))
         (eqs (parse-mkbtt-expression content)))
    (multiple-value-bind (flag ordering result)
        (multi-kb-completion eqs 15)
      (show-batch-completion-result flag ordering result)
      result)))


(defun main (args)
  (let ((subcommand (first args))
        (subcommand-args (cdr args)))
    (cond
      ((probe-file subcommand)
       ; execute default command for a file
       (%perform-command :COMPLETE (list subcommand)))
      ((or (not (scan "[a-zA-Z]+" subcommand))
           (> (length subcommand) 9))
       (%stdout "malformed command: ~A~%" subcommand)
       (help))
      (t
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
            (%stdout "unhandled condition occurred: ~A~%quit~%" con)))))))

