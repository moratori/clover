(defpackage clover.unify
  (:use :cl
        :clover.conditions
        :clover.util
        :clover.types)
  (:export 
    :unify)
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




(defmethod %collect-unifier-set-candidate ((term1 term) (term2 term))
  (unifier-set
    (%%collect-unifier-set-candidate term1 term2)))




(defmethod %%collect-unifier-set-candidate ((term1 vterm) (term2 vterm))
  (list 
    (unifier term1 term2)))


(defmethod %%collect-unifier-set-candidate ((term1 vterm) (term2 fterm))
  (when (%occurrence-check term1 term2)
    (error 
      (make-condition 'occurrence-check-error
                      :message "occurrence check error while %%collect-unifier-set-candidate"
                      :vterm term1
                      :fterm term2)))
  (list 
    (unifier term1 term2)))


(defmethod %%collect-unifier-set-candidate ((term1 fterm) (term2 vterm))
  (%collect-unifier-set-candidate term2 term1))


(defmethod %%collect-unifier-set-candidate ((term1 fterm) (term2 fterm))
  (let* ((fsymbol1 (fterm.fsymbol term1))
         (fsymbol2 (fterm.fsymbol term2))
         (args1    (fterm.args term1))
         (args2    (fterm.args term2))
         (length1  (length args1))
         (length2  (length args1)))
    (unless (and (eq fsymbol1 fsymbol2)
                 (=  length1  length2))
      (error (make-condition 'unmatching-fterm-error
                             :message "unmatching fterm error while %%collect-unifier-set-candidate"
                             :fterm1 term1
                             :fterm2 term2)))
    (loop 
      for arg1 in args1
      for arg2 in args2
      append (%%collect-unifier-set-candidate arg1 arg2))))


(defmethod %%collect-unifier-set-candidate ((literal1 literal) (literal2 literal))
  (let* ((predicate1 (literal.predicate literal1))
         (predicate2 (literal.predicate literal2))
         (args1      (literal.args literal1))
         (args2      (literal.args literal2))
         (length1    (length args1))
         (length2    (length args2)))

    (unless (and (eq predicate1 predicate2)
                 (= length1 length2))
      (error (make-condition 'unmatching-literal-error
                             :message "unmatching literal while %%collect-unifier-set-candidate"
                             :literal1 literal1
                             :literal2 literal2)))
    (loop 
      for arg1 in args1
      for arg2 in args2
      append (%%collect-unifier-set-candidate arg1 arg2))))



(defmethod unify ((literal1 literal) (literal2 literal))
  (let ((unifier-set-candidate 
          (handler-case 
              (%collect-unifier-set-candidate literal1 literal2)
            (unmatching-fterm-error (e)
              (declare (ignore e))
              (error (make-condition 'ununifiable-literal-error
                                     :message "ununifiable literal error because of unmathing fterm exists"
                                     :literal1 literal1
                                     :literal2 literal2)))
            (unmatching-literal-error (e)
              (declare (ignore e))
              (error (make-condition 'ununifiable-literal-error
                                     :message "ununifiable literal error because of unmathing literals"
                                     :literal1 literal1
                                     :literal2 literal2)))
            (occurrence-check-error (e)
              (declare (ignore e))
              (error (make-condition 'ununifiable-literal-error
                                     :message "ununifiable literal error because of occurrence check error"
                                     :literal1 literal1
                                     :literal2 literal2))))))
    ;; implement logic
    )
  )




