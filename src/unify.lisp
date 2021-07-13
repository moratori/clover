(defpackage clover.unify
  (:use :cl
        :clover.conditions
        :clover.util
        :clover.substitute
        :clover.types)
  (:export 
    :subsumption-clause-p
    :alphabet=
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


(defun %find-most-general-unifier-set (object1 object2)
  (handler-case

      (let* ((result 
               (%collect-disagreement-set object1 object2)))

        (loop :for   unifier := (%select-one-of-substitutable-unifier result)
              :while unifier
              :do    (setf result (%flatten-disagreement-set result unifier)))

        (unless (consistent-unifier-set-p result)
          (error 
            (make-condition 
              'ununifiable-error
              :message "ununifiable error because of unmathing fterm exists"
              :object1 object1
              :object2 object2)))

        result)

    (unmatching-fterm-error (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of unmathing fterm exists"
          :object1 object1
          :object2 object2)))
    (unmatching-literal-error (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of unmathing literals"
          :object1 object1
          :object2 object2)))
    (occurrence-check-error (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of occurrence check error"
          :object1 object1
          :object2 object2)))
    (unexpected-unifier-source (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of unexpected unifier source"
          :object1 object1
          :object2 object2)))))


(defmethod find-most-general-unifier-set ((literal1 literal) (literal2 literal))
  (unless (and (eq (literal.predicate literal1)
                   (literal.predicate literal2))
               (= (length (literal.args literal1))
                  (length (literal.args literal2))))
    (error 
      (make-condition 
        'ununifiable-error
        :message "ununifiable literal"
        :object1 literal1
        :object2 literal2)))
  (%find-most-general-unifier-set literal1 literal2))


(defmethod find-most-general-unifier-set ((fterm1 fterm) (fterm2 fterm))
  (unless (and (eq (fterm.fsymbol fterm1)
                   (fterm.fsymbol fterm2))
               (= (length (fterm.args fterm1))
                  (length (fterm.args fterm2))))
    (error 
      (make-condition 
        'ununifiable-error
        :message "ununifiable fterm"
        :object1 fterm1
        :object2 fterm2)))
  (%find-most-general-unifier-set fterm1 fterm2))


(defmethod subsumption-clause-p ((clause1 clause) (clause2 clause))
  ;; clause1がclause2を包摂するかを判定する
  ;; clause1がclause2を包摂する <=>
  ;; len(clause1) <= len(clause2) 且つ
  ;; ある代入S が存在して、clause1・S subset-of clause2
  (when (<= (clause-length clause1)
            (clause-length clause2))
    (let (unifier-set-list found-isolated-literal)
      (loop
        :named exit2
        :for lit1 :in (clause.literals clause1)
        :for first-found-mgu := 
        (loop
          :named exit1
          :for lit2 :in (clause.literals clause2)
          :for mgu := (handler-case
                         (find-most-general-unifier-set lit1 lit2)
                        (ununifiable-error (e) nil))
          :if mgu
          :do (return-from exit1 mgu))
        :if first-found-mgu
        :do (push first-found-mgu unifier-set-list)
        :if (not first-found-mgu)
        :do (progn
              (setf found-isolated-literal t)
              (return-from exit2)))
      (unless found-isolated-literal
        (let ((theta 
                (unifier-set
                  (mapcan 
                    (lambda (us)
                      (unifier-set.unifiers us))
                    unifier-set-list))))
          (and 
            (consistent-unifier-set-p theta)
            (clause-subset 
              (apply-unifier-set clause1 theta)
              clause2)))))))

(defmethod alphabet= ((term1 term) (term2 term))
  (let* ((dummy-pred
           (%intern-symbol-to-specified-package
             "DUMMYPRED"))
         (dummy-clause1 
          (clause
            (list (literal nil dummy-pred (list term1)))))
         (dummy-clause2
          (clause
            (list (literal nil dummy-pred (list term2))))))
    (and (subsumption-clause-p dummy-clause2 dummy-clause1)
         (subsumption-clause-p dummy-clause1 dummy-clause2))))

(defmethod alphabet= ((clause1 clause) (clause2 clause))
  (and (subsumption-clause-p clause2 clause1)
       (subsumption-clause-p clause1 clause2)))

(defmethod alphabet= ((clause-set1 clause-set) (clause-set2 clause-set))
  (and 
    (null (set-difference 
            (clause-set.clauses clause-set1)
            (clause-set.clauses clause-set2)
            :test #'alphabet-clause=))
    (null (set-difference 
            (clause-set.clauses clause-set2)
            (clause-set.clauses clause-set1)
            :test #'alphabet-clause=))))
