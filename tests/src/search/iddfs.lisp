(defpackage clover.tests.search.iddfs
  (:use :cl
        :clover.search.common
        :clover.search.iddfs
        :1am))
(in-package :clover.tests.search.iddfs)


(defstruct (even-or-odds 
             (:include abstract-node))
  (num 1 :type number))

(defmethod open-nodes ((node even-or-odds))
  (let ((parent (even-or-odds-num node)))
    (list 
      (make-even-or-odds :num (* 2 parent))
      (make-even-or-odds :num (1+  (* 2 parent))))))




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



(test clover.tests.search.iddfs.test1
  (defmethod finish ((node even-or-odds))
    (= 9 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (iddfs (make-even-or-odds :num 1) 10)

    (is (= deepth 3))
    (is (= (even-or-odds-num value) 9))))

(test clover.tests.search.iddfs.test2
  (defmethod finish ((node even-or-odds))
    (= 27 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (iddfs (make-even-or-odds :num 1) 10)

    (is (= deepth 4))
    (is (= (even-or-odds-num value) 27))))

(test clover.tests.search.iddfs.test3
  (defmethod finish ((node even-or-odds))
    (= 1234 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (iddfs (make-even-or-odds :num 1) 3)

    (is (null deepth))
    (is (null value))))

(test clover.tests.search.iddfs.test4
  (defmethod finish ((node even-or-odds))
    (= 1 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (iddfs (make-even-or-odds :num 1) 3)

    (is (= deepth 0))
    (is (= 1 (even-or-odds-num value)))))


(test clover.tests.search.iddfs.test5
  (defmethod finish ((node looped-graph))
    (string= "G" (looped-graph-label node)))
  (multiple-value-bind 
    (deepth value)
    (iddfs (make-looped-graph :label "A") 10)

    (is (= deepth 2))
    (is (string= "G" (looped-graph-label value)))))

(test clover.tests.search.iddfs.test6
  (defmethod finish ((node looped-graph))
    (string= "E" (looped-graph-label node)))
  (multiple-value-bind 
    (deepth value)
    (iddfs (make-looped-graph :label "A") 10)

    (is (= deepth 1))
    (is (string= "E" (looped-graph-label value)))))

(test clover.tests.search.iddfs.test7
  (let ((num 40342432))

    (defmethod finish ((node even-or-odds))
      (= num (even-or-odds-num node)))

    (multiple-value-bind 
        (deepth value)
        (iddfs (make-even-or-odds :num 1) 150)
      
      (is (= (even-or-odds-num value) num)))))


