(defpackage clover.tests.search.astar
  (:use :cl
        :clover.search.common
        :clover.search.astar
        :1am))
(in-package :clover.tests.search.astar)

(defparameter *mapping* 
  '(("S" . (("AO" . 10)))
            ("AO" . 
             (("S" . 10)
              ("IW" . 7)
              ("AK" . 4)))
            ("IW" . 
             (("AO" . 7)
              ("KO" . 6)))
            ("AK" . 
             (("AO" . 4)
              ("YM" . 6)))
            ("KO" . 
             (("IB" . 4)
              ("IW" . 6)
              ("NI" . 4)))
            ("YM" . 
             (("AK" . 6)
              ("NI" . 4)))
            ("NI" . 
             (("YM" . 4)
              ("KO" . 4)))
            ("IB" . 
             (("KO" . 4)))))

(defstruct (city
             (:include abstract-node)
             (:conc-name city.)
             (:constructor city (name from)))
  (name "" :type string)
  (from nil :type list)
  )

(defmethod finish ((city city))
  (string=
    (city.name city) "S"))

(defmethod cost-to-goal ((city city))
  2.5)

(defmethod cost-to-neighbor ((city1 city) (city2 city))
  (cdr (assoc (city.name city2)
         (cdr (assoc (city.name city1) *mapping* :test #'string=))
         :test #'string=)))

(defmethod open-nodes ((city city))
  (mapcar
    (lambda (x)
      (city (car x) (cons city (city.from city))))
    (cdr (assoc (city.name city) *mapping* :test #'string=))))



(test clover.tests.search.astar.test1
  (multiple-value-bind 
    (foundp value)
    (astar (city "IB" nil))
    (is value)))

