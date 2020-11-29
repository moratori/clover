(defpackage clover.clover
  (:use :cl
        :clover.property
        :clover.search.iddfs
        :clover.types
        :clover.resolution
        :clover.util
        )
  (:export
    :start_resolution
    ))
(in-package :clover.clover)


(defmethod open-nodes ((clause-set clause-set))
  (opener_clause-set clause-set 
                     (clause-set.resolution-mode clause-set)))


(defmethod finish ((clause-set clause-set))
  (some  #'null-clause-p
        (clause-set.clauses clause-set)))


(defmethod start_resolution ((clause-set clause-set))
  (when (find-if 
          (lambda (c) (null (clause.clause-type c)))
          (clause-set.clauses clause-set))
    (error "clause type mustn't be null"))
  (cond
    ((and (horn-clause-set-p clause-set)
          (find-if #'goal-clause-p 
                   (clause-set.clauses clause-set)))
     (horn-resolution clause-set))
    (t 
     (precipitately-resolution clause-set))))


(defmethod horn-resolution ((clause-set clause-set))
  (let ((clauses (clause-set.clauses clause-set)))
    (loop
      :named exit
      :for raw :in (remove-if-not #'goal-clause-p clauses)
      :for goal-clause := (clause 
                            (clause.literals raw)
                            (clause.parent1 raw)
                            (clause.parent2 raw)
                            (clause.unifier raw)
                            :goal)
      :for goal-clauses := (cons goal-clause 
                                 (remove raw
                                         clauses 
                                         :test #'clause=))
      :do 
      (multiple-value-bind 
          (cnt value) (iddfs (clause-set goal-clauses :horn-snl) 
                             *resolution-search-depth*)
        (when cnt
          (return-from exit (values cnt value)))))))

(defmethod precipitately-resolution ((clause-set clause-set))
  (let ((clauses (clause-set.clauses clause-set)))
   (loop
    :named exit
    :for max-depth :from 0 :upto *resolution-search-depth*
    :do
    (loop
      :for raw :in clauses
      :for centerlized-clause := (clause 
                                   (clause.literals raw)
                                   (clause.parent1 raw)
                                   (clause.parent2 raw)
                                   (clause.unifier raw)
                                   :center)
      :for centerlized-clauses := (cons centerlized-clause 
                                           (remove raw
                                                   clauses 
                                                   :test #'clause=))
      :do 
      (multiple-value-bind 
          (cnt value) (iddfs (clause-set centerlized-clauses
                                         :precipitately) 
                             max-depth)
        (when cnt
          (return-from exit (values cnt value))))))))

(defmethod default-resolution ((clause-set clause-set))
  (let ((clauses (clause-set.clauses clause-set)))
   (loop
    :named exit
    :for max-depth :from 0 :upto *resolution-search-depth*
    :do
    (loop
      :for raw :in clauses
      :for centerlized-clause := (clause 
                                   (clause.literals raw)
                                   (clause.parent1 raw)
                                   (clause.parent2 raw)
                                   (clause.unifier raw)
                                   :center)
      :for centerlized-clauses := (cons centerlized-clause 
                                           (remove raw
                                                   clauses 
                                                   :test #'clause=))
      :do 
      (multiple-value-bind 
          (cnt value) (iddfs (clause-set centerlized-clauses
                                         :default) 
                             max-depth)
        (when cnt
          (return-from exit (values cnt value))))))))

