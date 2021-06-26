(defpackage clover.search.common
  (:use :cl)
  (:export 
    :abstract-node
    :open-nodes
    :finish
    :node-hash
    :node-equality
    :*debug-print*
    )
  )
(in-package :clover.search.common)


(defparameter *debug-print* nil)

(defstruct abstract-node)

(defmethod open-nodes ((node abstract-node))
  ;; abstract-node のリストを返す関数
  (error "implement for specific method"))

(defmethod finish ((node abstract-node))
  ;; abstract-node を引数にとって t or nil を返す関数
  (error "implement for specific method"))
 
(defmethod node-hash ((node abstract-node))
  ;; abstract-nodeのハッシュ値を返す関数
  (error "implement for specific method")
  )

(defmethod node-equality (node1 abstract-node) (node2 abstract-node)
  (error "implement for specific method"))
