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

;; dfs の再訪チェックは node-canonical-key(変数リネーム不変な正準キー)を equal ハッシュの
;; キーに使う。looped-graph はラベルがそのまま状態を一意に表すため、ラベル文字列を返す。
(defmethod node-canonical-key ((node looped-graph))
  (looped-graph-label node))


;; 到達可能なゴール(G)を発見できる。flag が真であることも確認する。
(test clover.tests.search.dfs.test1
  (defmethod finish ((node looped-graph))
    (string= "G" (looped-graph-label node)))
  (multiple-value-bind
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is flag)
    (is (string= "G" (looped-graph-label value)))))

;; 到達可能なゴール(E)を発見できる。flag が真であることも確認する。
(test clover.tests.search.dfs.test2
  (defmethod finish ((node looped-graph))
    (string= "E" (looped-graph-label node)))
  (multiple-value-bind
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is flag)
    (is (string= "E" (looped-graph-label value)))))

;; 初期ノード自身がゴールの場合: 初手で finish が真になり、その初期ノードを返す。
(test clover.tests.search.dfs.test3
  (defmethod finish ((node looped-graph))
    (string= "A" (looped-graph-label node)))
  (multiple-value-bind
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is flag)
    (is (string= "A" (looped-graph-label value)))))

;; 到達不能なゴール(Z): 閉路を含むグラフでも停止し、(nil nil) を返す。
;; 再訪チェックが機能していなければ無限ループするため、本テストは停止性の検証も兼ねる。
(test clover.tests.search.dfs.test4
  (defmethod finish ((node looped-graph))
    (string= "Z" (looped-graph-label node)))
  (multiple-value-bind
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is (null flag))
    (is (null value))))

;; 深い位置のノード(D は A->B->D のみで到達可能)を発見できる。
(test clover.tests.search.dfs.test5
  (defmethod finish ((node looped-graph))
    (string= "D" (looped-graph-label node)))
  (multiple-value-bind
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is flag)
    (is (string= "D" (looped-graph-label value)))))

;; 別経路のノード(F)も発見できる。
(test clover.tests.search.dfs.test6
  (defmethod finish ((node looped-graph))
    (string= "F" (looped-graph-label node)))
  (multiple-value-bind
    (flag value)
    (dfs (make-looped-graph :label "A"))

    (is flag)
    (is (string= "F" (looped-graph-label value)))))


;; ---------------------------------------------------------------------------
;; 動的に生成する適度に大きな探索空間 (改修前後の性能変動を観察するための固定ワークロード)
;;
;; (0,0) を起点とする N×N 格子。各ノード (i,j) は右(i+1,j)と上(i,j+1)へ辺を持つ。
;; 同一ノード (i,j) へは「左から」と「下から」の複数経路で到達しうるため、再訪チェックが
;; 無ければ展開数が経路数=組合せ的に爆発する。再訪チェックが機能していれば展開は高々
;; (N+1)^2 ノードに収まる。したがって本空間は再訪チェックの効きと、そのハッシュ機構の
;; コストを観察するのに適している。
;; ---------------------------------------------------------------------------

(defparameter *grid-size* 120)

(defstruct (grid-node
             (:include abstract-node))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defmethod open-nodes ((node grid-node))
  (let ((x (grid-node-x node))
        (y (grid-node-y node))
        (n *grid-size*)
        (acc '()))
    (when (< x n) (push (make-grid-node :x (1+ x) :y y) acc))
    (when (< y n) (push (make-grid-node :x x :y (1+ y)) acc))
    acc))

;; 座標 (x,y) がそのまま状態を一意に表すため、それを正準キーとする(equal で内容比較される)。
(defmethod node-canonical-key ((node grid-node))
  (cons (grid-node-x node) (grid-node-y node)))

;; 到達不能ゴール: どのノードもゴールでないため全 (N+1)^2 ノードを探索しきって停止する。
;; 再訪チェックが効いていなければ経路爆発 or 無限ループとなるため、停止性の検証も兼ねる。
;; (改修前後の性能を比較する際の重い固定ワークロード)
(test clover.tests.search.dfs.large-unreachable
  (defmethod finish ((node grid-node))
    nil)
  (multiple-value-bind
    (flag value)
    (dfs (make-grid-node :x 0 :y 0))

    (is (null flag))
    (is (null value))))

;; 到達可能ゴール(右上隅 (N,N)): 大きな格子でも正しく発見できる。
(test clover.tests.search.dfs.large-reachable
  (defmethod finish ((node grid-node))
    (and (= (grid-node-x node) *grid-size*)
         (= (grid-node-y node) *grid-size*)))
  (multiple-value-bind
    (flag value)
    (dfs (make-grid-node :x 0 :y 0))

    (is flag)
    (is (and (= (grid-node-x value) *grid-size*)
             (= (grid-node-y value) *grid-size*)))))

