(defpackage clover.lib.search.common
  (:use :cl
        )
  (:export
    :abstract-node
    :open-nodes
    :finish
    :node-canonical-key
    :*debug-print*
    )
  )
(in-package :clover.lib.search.common)


(defparameter *debug-print* nil)

(defstruct abstract-node)

(defmethod open-nodes ((node abstract-node))
  ;; abstract-node のリストを返す関数
  (error "implement for specific method"))

(defmethod finish ((node abstract-node))
  ;; abstract-node を引数にとって t or nil を返す関数
  (error "implement for specific method"))
 
(defmethod node-canonical-key ((node abstract-node))
  ;; 変数リネームに不変な正準キー(equalで比較可能な値)を返す。
  ;; 同一(変種)状態の重複展開を抑制する closed 集合のキーとして用いる。
  ;; デフォルトはノード自身を返す。equal ハッシュ上では構造体は eq 比較となるため、
  ;; 正準化を実装しないノード型では「同一オブジェクトのみ一致」=従来どおり実質重複排除なし、
  ;; という安全側の挙動になる(専用メソッドを実装した型だけが正準キーで重複排除される)。
  node)
