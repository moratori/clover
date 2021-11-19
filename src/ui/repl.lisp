(defpackage clover.ui.repl
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.rendertree
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
(in-package :clover.ui.repl)

(defparameter *current-axiomatic-system* nil
  "name of current axiomatic-system")

(defparameter *axiomatic-system-list* nil
  "assoc list of (axiomatic-system-name . clause-set)")

(defparameter *completed-system-list* nil
  "assoc lis of (axiomatic-system-name . rewrite-rule-set)")

(defparameter *render-tree-path-name* nil
  "file path to output refutation tree")



(defmethod %perform-command ((command (eql :SKIPP)) args))

(defmethod %perform-command ((command (eql :EXIT)) args)
  (throw 'exit nil))

(defmethod %perform-command ((command (eql :QUIT)) args)
  (throw 'exit nil))

(defmethod %perform-command ((command (eql :HELP)) args)
  (%stdout
   "Command help
    :help                    show this help
    :quit                    quit from REPL
    :def-axiom    <name>     define an axiomatic system <name>
    :show-axiom              enumerate all axiomatic system that are currently defined
    :set-axiom    <name>     set current axiomatic system to <name>
    :save-axiom   <path>     save curent axiomatic system to <path>
    :load-axiom   <path>     restore axiomatic systems from <path>
    :set-history             keep resolution history. this option automatically 
                             enabled if save-tree option on.
    :unset-history           disable history
    :save-tree    <path>     save Graphviz code to <path>
    :unsave-tree~%"))

(defmethod update-axiomatic-system ((name string) (clause-set clause-set))
  (setf *axiomatic-system-list*
          (cons (cons name clause-set) 
                (remove-if 
                  (lambda (x)
                    (destructuring-bind (current-name . _) x
                      (declare (ignore _))
                      (string= current-name name)))
                  *axiomatic-system-list*))
          *current-axiomatic-system*
          (first (assoc name *axiomatic-system-list* :test #'string=))))

(defmethod update-axiomatic-system ((name string) (rewrite-rule-set rewrite-rule-set))
  (setf *completed-system-list*
          (cons (cons name rewrite-rule-set) 
                (remove-if 
                  (lambda (x)
                    (destructuring-bind (current-name . _) x
                      (declare (ignore _))
                      (string= current-name name)))
                  *completed-system-list*))
          *current-axiomatic-system*
          (first (assoc name *completed-system-list* :test #'string=))))


(defmethod prompt-and-do-completion ((equation-set equation-set) (name string))
  (%stdout "Detected that a set of equations has been inputted.")
  (when (yes-or-no-p "Do you want to execute completion algorithm? ")
    (multiple-value-bind (flag ordering completed)
          (multi-kb-completion equation-set 15)
        (cond
          (flag
           (%stdout "~%The completion process was successful: ~%")
           (%stdout "function ordering : ~A~%~%" ordering)
           (loop
             :for rule :in (rewrite-rule-set.rewrite-rules
                             (rename-for-human-readable-printing completed))
             :do (%stdout "~A~%" rule))
           (update-axiomatic-system name completed))
          (t 
           (%stdout "The completion process failed~%"))))))



(defmethod %perform-command ((command (eql :SHOW-AXIOM)) args)
  (loop
    :for (name . ax) :in *axiomatic-system-list*
    :for completed := (assoc name *completed-system-list* :test #'string=)
    :do
    (progn
      (%stdout "=====Axiom: ~A=====~%~A~%" name ax)
      (when completed
        (%stdout "completed axiom:~%")
        (loop
          :for each :in (rewrite-rule-set.rewrite-rules 
                          (rename-for-human-readable-printing
                            (cdr completed)))
          :do 
          (%stdout "~A~%" each))
        (%stdout "~%")))))


(defmethod %perform-command :around ((command (eql :DEF-AXIOM)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args))
          (scan "[a-zA-Z0-9\.]+" (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))

(defmethod %perform-command ((command (eql :DEF-AXIOM)) args)

  (let* ((name (first args))
         (cnt 1) 
         clauses)

    (%stdout "input . to finish definition~%")
    (%prompt-def cnt)
    
    (loop
      :for line   = (%read-line-with-sigint-guard)
      :while        (and line (not (string= "."
                                           (string-trim '(#\Space #\Tab #\Newline #\Return) line))))
      :for parsed = (handler-case 
                        (parse-premise-logical-expression line)
                      (expr-parse-error (con)
                        (%stdout "parser error: ~A~%" con)))
      :do
      (when parsed
        (setf cnt (1+ cnt)
              clauses (push parsed clauses)))
      (%prompt-def cnt))

    (let* ((cs (clause-set clauses))
           (es (convert-to-equation-set cs)))
      (update-axiomatic-system name cs)
      (when es
        (prompt-and-do-completion es name)))))



(defmethod %perform-command :around ((command (eql :SET-AXIOM)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args))
          (scan "[a-zA-Z0-9\.]+" (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))

(defmethod %perform-command ((command (eql :SET-AXIOM)) args)
  (let ((name (first args)))
    (setf *current-axiomatic-system*
          (first (assoc name *axiomatic-system-list* :test #'string=)))))




(defmethod %perform-command :around ((command (eql :DEFAULT)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))


(defmethod do-resolution ((clause-set clause-set))
  (multiple-value-bind (depth clause-set)
      (handler-case
          (time (start_resolution clause-set))
        (clover-toplevel-condition (con)
          (%stdout "unexpected error occurred: ~A~%" con)
          (throw 'exit nil))
        (condition (con)
          (%stdout "caught an signal : ~A~%" con)
          (%stdout "process canceled~%~%")
          (values nil nil)))
    (cond 
      (depth
       (%stdout "~A under the ~A~%~%"
                (make-bold-string "PROVABLE")
                *current-axiomatic-system*)
       (when *render-tree-path-name*
         (render-refutation-tree clause-set *render-tree-path-name*))
       (when *save-resolution-history*
         (render-refutation-tree clause-set *standard-output*)))
      (t
       (%stdout "unknown provability under the ~A~%~%"
                *current-axiomatic-system*)))))

(defmethod do-trs ((clause clause) (rewrite-rule-set rewrite-rule-set))
  (let* ((literals (clause.literals clause))
         (irreducibles nil)
         (ret
           (every
             (lambda (eqs)
               (let* ((left (equation.left eqs))
                      (right (equation.right eqs))
                      (tmp (equation (not (equation.negation eqs))
                                     left
                                     right)))
                 (multiple-value-bind (r v) (start_trs tmp rewrite-rule-set)
                   (push v irreducibles)
                   r)))
             literals)))
    (if ret
        (progn
          (%stdout "The equation can be ~A under the axiom ~A~%"
                   (make-bold-string "PROVED")
                   *current-axiomatic-system*)
          (%stdout "~%irreducible form under the ~A:~%"
                   *current-axiomatic-system*)
          (loop
            :for each :in irreducibles
            :do
            (%stdout "~A~%" each)))
        (%stdout "The equation cannot be proved under axioms ~A~%"
                 *current-axiomatic-system*))))


(defmethod %perform-command ((command (eql :DEFAULT)) args)
  (let* ((line (first args))
         (expr (parse-conseq-logical-expression line))
         (axiomatic-system 
           (cdr (assoc *current-axiomatic-system*
                       *axiomatic-system-list* :test #'string=)))
         (clauses
           (when axiomatic-system
             (clause-set.clauses axiomatic-system)))
         (completed
           (cdr (assoc *current-axiomatic-system*
                       *completed-system-list* :test #'string=)))
         (is-equation
           (every
             (lambda (x)
               (typep x 'equation))
             (clause.literals expr))))

    (when *render-tree-path-name*
      (setf *save-resolution-history* t))

    (catch 
      'exit 
      (if (and completed is-equation)
          (do-trs expr completed)
          (do-resolution 
            (clause-set (cons expr clauses)))))))


(defmethod %perform-command :around ((command (eql :SAVE-AXIOM)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args))
          (pathname (first args))
          (scan "[a-zA-Z0-9\.]+" (pathname-name (pathname (first args)))))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))

(defmethod %perform-command ((command (eql :SAVE-AXIOM)) args)
  (let ((output
          (pathname (first args))))
    (unless *current-axiomatic-system*
      (%stdout "current axiomatic system is null"))
    (when *current-axiomatic-system*
      (with-open-file (handle output :direction :output :if-exists :supersede)
        (let ((axiomatic-system
                (cdr 
                  (assoc *current-axiomatic-system*
                         *axiomatic-system-list* :test #'string=))))
          (format handle "~A" axiomatic-system)))
      (%stdout "save completed~%"))))


(defmethod %perform-command :around ((command (eql :LOAD-AXIOM)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args))
          (pathname (first args))
          (scan "[a-zA-Z0-9\.]+" (pathname-name (pathname (first args)))))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))

(defmethod %perform-command ((command (eql :LOAD-AXIOM)) args)
  (let* ((input-fname
          (pathname (first args)))
         (fname 
           (pathname-name input-fname)))
    (catch 
      'exit
      (with-open-file (handle input-fname :direction :input :if-does-not-exist nil)
        (unless handle
          (%stdout "file not found: ~A~%" input-fname)
          (throw 'exit nil))
        (when handle
          (let ((clauses
                (loop
                  :for line := (read-line handle nil nil)
                  :while line
                  :for trimmed := (string-trim '(#\Space #\Tab #\Newline #\Return) line)
                  :if (and (> (length trimmed) 0)
                           (char/= (char trimmed 0) #\#))
                  :collect 
                  (handler-case 
                      (parse-premise-logical-expression trimmed)
                    (expr-parse-error (con)
                      (%stdout "parser error: ~A~%~A~%" con trimmed)
                      (throw 'exit nil))))))

            (update-axiomatic-system fname (clause-set clauses))
            (%stdout "load completed~%")

            (let* ((cs (clause-set clauses))
                   (es (convert-to-equation-set cs)))
              (when es
                (prompt-and-do-completion es fname)))))))))


(defmethod %perform-command :around ((command (eql :SAVE-TREE)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args)))
     (call-next-method))
    (t 
     (%stdout "invalid command argument: ~A~%" args))))


(defmethod %perform-command ((command (eql :SAVE-TREE)) args)
  (setf *render-tree-path-name* (pathname (first args))))

(defmethod %perform-command ((command (eql :UNSAVE-TREE)) args)
  (setf *render-tree-path-name* nil))
 
(defmethod %perform-command ((command (eql :set-history)) args)
  (setf *save-resolution-history* t))

(defmethod %perform-command ((command (eql :unset-history)) args)
  (setf *save-resolution-history* nil))


(defun main ()
  (%perform-command :help nil)
  (%prompt-toplevel *current-axiomatic-system*) 
  (catch 'exit
      (handler-case 
          (loop 
              :for   line           = (%read-line-with-sigint-guard)
              :while line
              :for   trimmed        = (string-trim '(#\Space #\Tab #\Newline #\Return) line)
              :for   (command args) = (multiple-value-list (%parse-input-line trimmed))
              :for   next-method    = (compute-applicable-methods #'%perform-command (list command args))
              :do 
              (handler-case
                  (progn
                    (cond 
                      (next-method
                       (%perform-command command args))
                      (t 
                       (%stdout "command not found: ~A~%" command))))
                (clover-toplevel-condition (con)
                  (%stdout "~A~%" con)))
              (%prompt-toplevel *current-axiomatic-system*))
          (condition (con)
            (%stdout "unhandled condition occurred: ~A~%quit~%" con)))))

