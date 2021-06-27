(defpackage clover.search.common
  (:use :cl
        :cl-custom-hash-table
        )
  (:export 
    :abstract-node
    :open-nodes
    :finish
    :node-hash
    :node-equality
    :node-same-class
    :make-equal-node-hash-table
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

(defmethod node-equality ((node1 abstract-node) (node2 abstract-node)) 
  (error "implement for specific method"))

(defmethod node-same-class ((node1 abstract-node) (node2 abstract-node))
  "字句単位では異なるnodeであるが、論理的には同じものであるnodeを識別する
   例: {P(x)|Q(x), !Q(y)} same class {P(w)|Q(w), !Q(y)}"
  ;; デフォルトでは node-equality としておく
  (node-equality node1 node2))

(define-custom-hash-table-constructor make-equal-node-hash-table
  :test node-equality 
  :hash-function node-hash)

;; same-classに対するハッシュテーブルも定義したいが、
;; hash関数を定義するのが難しい為、先送り
