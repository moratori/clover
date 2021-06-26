(defpackage clover.tests.search.dfs
  (:use :cl
        :clover.search.common
        :clover.search.dfs
        :1am))
(in-package :clover.tests.search.dfs)


(defstruct (looped-graph
             (:include abstract-node))
  (label "A" :type string))

(defmethod open-nodes ((node looped-graph))
  (let* ((val (looped-graph-label node))
         (nexts (cdr 
                   (assoc 
                     val 
                     '(("A" . ("B" "C" "E"))
                       ("B" . ("A" "D" "F"))
                       ("C" . ("A" "G"))
                       ("D" . ("B"))
                       ("E" . ("A" "F"))
                       ("F" . ("B" "E"))
                       ("G" . ("C")))
                     :test #'string=))))
    (mapcar 
      (lambda (x)
        (make-looped-graph
          :label x))
      nexts)))

(defmethod node-hash ((node looped-graph))
  (sxhash (looped-graph-label node)))

(defmethod node-equality ((node1 looped-graph) (node2 looped-graph))
  (string= (looped-graph-label node1)
           (looped-graph-label node2)))


(test clover.tests.search.dfs.test1
  (defmethod finish ((node looped-graph))
    (string= "G" (looped-graph-label node)))
  (multiple-value-bind 
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is (string= "G" (looped-graph-label value)))))

(test clover.tests.search.dfs.test2
  (defmethod finish ((node looped-graph))
    (string= "E" (looped-graph-label node)))
  (multiple-value-bind 
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is (string= "E" (looped-graph-label value)))))

