(defpackage clover.tests.search.extractor
  (:use :cl
        :clover.search.common
        :clover.search.extractor
        :1am))
(in-package :clover.tests.search.extractor)


(defstruct (even-or-odds 
             (:include abstract-node)
             (:constructor even-or-odds (&optional num)))
  (num 1 :type number))

(defmethod open-nodes ((node even-or-odds))
  (let ((parent (even-or-odds-num node)))
    (list 
      (even-or-odds (* 2 parent))
      (even-or-odds (1+  (* 2 parent))))))

(defmethod node-equality ((a even-or-odds) (b even-or-odds))
  (= (even-or-odds-num a)
     (even-or-odds-num b)))

(defmethod node-hash ((a even-or-odds))
  (sxhash (even-or-odds-num a)))



(test clover.tests.search.iddfs.test1
  (is 
    (let ((expected 
            (list 
              (even-or-odds 1)))
          (result
            (extract (even-or-odds) 0 t)))
      (and 
        (null (set-difference expected result :test #'node-equality))
        (null (set-difference result expected :test #'node-equality)))))

  (is 
    (let ((expected 
            (list 
              (even-or-odds 1)
              (even-or-odds 2)
              (even-or-odds 3)))
          (result
            (extract (even-or-odds) 1 t)))
      (and 
        (null (set-difference expected result :test #'node-equality))
        (null (set-difference result expected :test #'node-equality)))))

  (is 
    (let ((expected 
            (list 
              (even-or-odds 1)
              (even-or-odds 2)
              (even-or-odds 3)
              (even-or-odds 4)
              (even-or-odds 5)
              (even-or-odds 6)
              (even-or-odds 7)))
          (result
            (extract (even-or-odds) 2 t)))
      (and 
        (null (set-difference expected result :test #'node-equality))
        (null (set-difference result expected :test #'node-equality)))))
  )
