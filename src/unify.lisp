(defpackage clover.unify
  (:use :cl
        :clover.conditions
        :clover.util
        :clover.substitute
        :clover.types)
  (:export 
    :find-most-general-unifier-set)
  )
(in-package :clover.unify)


(defmethod %occurrence-check ((term1 term) (term2 term))
  nil)

(defmethod %occurrence-check ((term1 vterm) (term2 vterm))
  (term= term1 term2))

(defmethod %occurrence-check ((term1 vterm) (term2 fterm))
  (some
    (lambda (arg)
      (%occurrence-check term1 arg))
    (fterm.args term2)))



(defun %collect-disagreement-set (obj1 obj2)
  (let ((result
          (unifier-set
            (remove-duplicates 
              (%%collect-disagreement-set obj1 obj2)
              :test #'unifier=)))) 
    result))



(defmethod %%collect-disagreement-set ((term1 vterm) (term2 vterm))
  (list 
    (unifier term1 term2)))


(defmethod %%collect-disagreement-set ((term1 vterm) (term2 fterm))
  (when (%occurrence-check term1 term2)
    (error 
      (make-condition 'occurrence-check-error
                      :message "occurrence check error while %%collect-disagreement-set"
                      :vterm term1
                      :fterm term2)))
  (list 
    (unifier term1 term2)))


(defmethod %%collect-disagreement-set ((term1 fterm) (term2 vterm))
  (%%collect-disagreement-set term2 term1))


(defmethod %%collect-disagreement-set ((term1 fterm) (term2 fterm))
  (let* ((fsymbol1 (fterm.fsymbol term1))
         (fsymbol2 (fterm.fsymbol term2))
         (args1    (fterm.args term1))
         (args2    (fterm.args term2))
         (length1  (length args1))
         (length2  (length args1)))

    (unless (and (eq fsymbol1 fsymbol2)
                 (=  length1  length2))
      (error (make-condition 'unmatching-fterm-error
                             :message "unmatching fterm error while %%collect-disagreement-set"
                             :fterm1 term1
                             :fterm2 term2)))

    (mapcan #'%%collect-disagreement-set args1 args2)))


(defmethod %%collect-disagreement-set ((literal1 literal) (literal2 literal))
  (let* ((predicate1 (literal.predicate literal1))
         (predicate2 (literal.predicate literal2))
         (args1      (literal.args literal1))
         (args2      (literal.args literal2))
         (length1    (length args1))
         (length2    (length args2)))

    (unless (and (eq predicate1 predicate2)
                 (= length1 length2))
      (error (make-condition 'unmatching-literal-error
                             :message "unmatching literal while %%collect-disagreement-set"
                             :literal1 literal1
                             :literal2 literal2)))

    (mapcan #'%%collect-disagreement-set args1 args2)))



(defmethod %select-one-of-substitutable-unifier ((disagreement-set unifier-set))
  (let ((unifiers 
          (unifier-set.unifiers disagreement-set)))
    (find-if
      (lambda (target)
        (find-if
          (lambda (x)
            (and 
              (not (unifier= target x))
              (or
                (%occurrence-check (unifier.src target) (unifier.src x))
                (%occurrence-check (unifier.src target) (unifier.dst x)))))
          unifiers))
      unifiers)))


(defmethod %flatten-disagreement-set ((disagreement-set unifier-set) (unifier unifier))
  (unifier-set
    (remove-duplicates
      (mapcar 
        (lambda (target)
          (let* ((target-src (unifier.src target))
                 (target-dst (unifier.dst target))
                 (try1       (handler-case 
                                 (apply-unifier target unifier)
                               (unexpected-unifier-source (e) nil)))
                 (try2       (handler-case 
                                 (when (typep target-dst 'vterm)
                                   (apply-unifier 
                                     (unifier target-dst target-src)
                                     unifier))
                               (unexpected-unifier-source (e) nil)))
                 (result     (or try1 try2)))

            (cond 
              ((unifier= unifier target)
               target)
              ((not result)
               (error
                 (make-condition 'unexpected-unifier-source)))
              ((%occurrence-check (unifier.src result) (unifier.dst result))
               (error (make-condition 'occurrence-check-error
                                      :message "occurrence check error while %flatten-disagreement-set"
                                      :vterm (unifier.src result)
                                      :fterm (unifier.dst result))))
              (t result))))
        (unifier-set.unifiers disagreement-set))
      :test #'unifier=)))



(defmethod find-most-general-unifier-set ((literal1 literal) (literal2 literal))
  (unless (and (eq (literal.predicate literal1)
                   (literal.predicate literal2))
               (not (eq (literal.negation literal1)
                        (literal.negation literal2)))
               (= (length (literal.args literal1))
                  (length (literal.args literal2))))
    (error (make-condition 'ununifiable-literal-error
                           :message "ununifiable literal"
                           :literal1 literal1
                           :literal2 literal2)))
  (handler-case

      (let* ((result (%collect-disagreement-set literal1 literal2)))

        (loop :for   unifier = (%select-one-of-substitutable-unifier result)
              :while unifier
              :do    (setf result (%flatten-disagreement-set result unifier)))

        result)

    (unmatching-fterm-error (e)
      (error (make-condition 'ununifiable-literal-error
                             :message "ununifiable literal error because of unmathing fterm exists"
                             :literal1 literal1
                             :literal2 literal2)))
    (unmatching-literal-error (e)
      (error (make-condition 'ununifiable-literal-error
                             :message "ununifiable literal error because of unmathing literals"
                             :literal1 literal1
                             :literal2 literal2)))
    (occurrence-check-error (e)
      (error (make-condition 'ununifiable-literal-error
                             :message "ununifiable literal error because of occurrence check error"
                             :literal1 literal1
                             :literal2 literal2)))
    (unexpected-unifier-source (e)
      (error (make-condition 'ununifiable-literal-error
                             :message "ununifiable literal error because of unexpected unifier source"
                             :literal1 literal1
                             :literal2 literal2)))))

