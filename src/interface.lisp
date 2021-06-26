(defpackage clover.interface
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.rendertree
        :clover.types
        :clover.parser
        :clover.util
        :clover.clover
        :ppcre
        )
  (:export 
    :main
    ))
(in-package :clover.interface)

(defparameter *current-axiomatic-system* nil
  "name of current axiomatic-system")

(defparameter *axiomatic-system-list* nil
  "assoc list of (axiomatic-system-name . clause-set)")

(defparameter *render-tree-path-name* nil
  "file path to output refutation tree")

(defparameter *statistical-profiler* nil
  "whether to profile resolution process")


(defmacro %stdout (control-string &rest values)
  `(progn
     (format *standard-output* ,control-string ,@values)
     (force-output *standard-output*)))

(defun %prompt-toplevel ()
  (%stdout 
    (make-bold-string 
      (format nil "(~A)>>> " *current-axiomatic-system*))))

(defun %prompt-def (counter)
  (%stdout 
    (make-bold-string
      (format nil "axiom[~A]>>> " counter))))

(defun %read-line-with-sigint-guard ()
  (handler-case
      (read-line *standard-input* nil nil)
    (condition (con) 
      (declare (ignore con))
      nil)))

(defun make-bold-string (str)
    (format nil "~C[~Am~A~C[0m" 
            (code-char #o33) 
            "1" 
            str
            (code-char #o33)))


(defmethod %perform-command ((command (eql :SKIPP)) args))

(defmethod %perform-command ((command (eql :EXIT)) args)
  (throw 'exit nil))

(defmethod %perform-command ((command (eql :QUIT)) args)
  (throw 'exit nil))

(defmethod %perform-command ((command (eql :HELP)) args)
  ;; todo: implement :save-axiom and :load-axiom
  (%stdout
   "Command help
    :help                    show this help
    :quit                    quit from REPL
    :def-axiom    <name>     define an axiomatic system <name>
    :show-axiom              enumerate all axiomatic system that are currently defined
    :set-axiom    <name>     set current axiomatic system to <name>
    :set-history             keep resolution history. this option automatically 
                             enabled if save-tree option on.
    :unset-history           disable history
    :set-profiler            enable statistical profiler
    :unset-profiler          disable statistical profiler
    :save-tree    <path>     save Graphviz code to <path>
    :unsave-tree~%"))


(defmethod %perform-command ((command (eql :SHOW-AXIOM)) args)
  (loop
    :for (name . ax) :in *axiomatic-system-list*
    :do
    (%stdout "===Axiom: ~A===~%~A~%" name ax)))


(defmethod %perform-command ((command (eql :set-profiler)) args)
  (setf *statistical-profiler* t)
  (%stdout "statistical profiler enabled~%"))

(defmethod %perform-command ((command (eql :unset-profiler)) args)
  (setf *statistical-profiler* nil)
  (%stdout "statistical profiler disabled~%"))


(defmethod %perform-command :around ((command (eql :DEF-AXIOM)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args))
          (scan "[a-zA-Z0-9]+" (first args)))
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
    
    (setf *axiomatic-system-list*
          (cons (cons name (clause-set clauses)) 
                (remove-if 
                  (lambda (x)
                    (destructuring-bind (current-name . _) x
                      (declare (ignore _))
                      (string= current-name name)))
                  *axiomatic-system-list*))
          *current-axiomatic-system*
          (first (assoc name *axiomatic-system-list* :test #'string=)))))




(defmethod %perform-command :around ((command (eql :SET-AXIOM)) args)
  (cond 
    ((and 
          (= 1 (length args))
          (stringp (first args))
          (scan "[a-zA-Z0-9]+" (first args)))
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

(defmethod %perform-command ((command (eql :DEFAULT)) args)
  (let* ((line (first args))
         (expr (parse-conseq-logical-expression line))
         (axiomatic-system 
           (cdr (assoc *current-axiomatic-system* *axiomatic-system-list* :test #'string=)))
         (clauses
           (when axiomatic-system
             (clause-set.clauses axiomatic-system))))

    (when *render-tree-path-name*
      (setf *save-resolution-history* t))

    (catch 'exit 
        (multiple-value-bind (depth clause-set)
            (handler-case
                (if *statistical-profiler*
                  (progn
                    #+sbcl 
                    (sb-sprof:with-profiling 
                      (:max-samples 3000
                       :report :flat
                       :loop nil
                       :mode :cpu
                       :show-progress nil)
                      (time (start_resolution 
                              (clause-set (cons expr clauses)))))
                    #-sbcl 
                    (time (start_resolution 
                            (clause-set (cons expr clauses)))))
                  (time (start_resolution 
                          (clause-set (cons expr clauses)))))
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
                      *current-axiomatic-system*)))))))



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


(defun %parse-input-line (line)
  (multiple-value-bind 
      (match-start match-end reg-starts reg-ends)
      (scan "^:[a-z\\-]+" line)
    (declare (ignore reg-starts reg-ends))
    (let ((command 
            (when match-start
              (subseq line (1+ match-start) match-end)))
          (args
            (when match-start
              (remove-if
                (lambda (x) (zerop (length x)))
                (ppcre:split " " (subseq line match-end))))))
      (cond 
        ((string= line "")
         (values :SKIPP (list line)))
        ((null match-start)
         (values :DEFAULT (list line)))
        (t 
         (values
           (intern (string-upcase command) (find-package "KEYWORD"))
           args))))))


(defun main ()

  (%perform-command :help nil)

  (%prompt-toplevel)

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
              (%prompt-toplevel))
          (condition (con)
            (%stdout "unhandled condition occurred: ~A~%quit~%" con)))))

