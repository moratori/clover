(defpackage clover.heuristics
  (:use :cl
        :clover.parameters
        :clover.lib.search.astar
        :clover.types
        )
  (:import-from :alexandria
                :median
                :variance
                :mean)
  (:import-from :clover.equality
                :term=)
  (:import-from :clover.logical-predicates
                :collect-variables
                :clause-length))
(in-package :clover.heuristics)


(defun %center-length (node)
  "残り歩数の見積り = center 節に残っているリテラル数"
  (let ((c (find-if (lambda (x) (eq :center (clause.clause-type x)))
                    (clause-set.clauses node))))
    (if c 
        (clause-length c) 
        (loop :for x :in (clause-set.clauses node)
              :minimize (clause-length x)))))


(defun %term-size (term)
  ;; 項の構文木の総ノード数(記号出現数)。
  ;; constant は fterm のサブタイプのため fterm より先に分岐する。
  (typecase term
    (vterm 1)
    (constant 1)
    (fterm (1+ (reduce #'+ (fterm.args term)
                       :key #'%term-size :initial-value 0)))))

(defun %clause-size (clause)
  ;; 節の「パターン面積」= 全リテラルの引数の構文木ノード数の合計。
  ;; 述語記号は数えない。
  (loop :for literal :in (clause.literals clause)
        :sum (loop :for arg :in (literal.args literal)
                   :sum (%term-size arg))))

(defun %clause-distinct-variable-count (clause)
  ;; 節内の相異なる変数の個数。
  ;; 同一変数の再出現は「初出と同じものであれ」という等値制約であり、
  ;; 自由度を増やさない(むしろ絞る)ため数えない。
  (length
    (remove-duplicates
      (mapcan #'collect-variables (clause.literals clause))
      :test #'term=)))

(defun %clause-permissiveness (clause)
  ;; 節の「スカスカ度」(素通し度) = distinct変数数 / パターン面積。
  ;; 単一化の観点で、非変数ノードは相手候補を絞る「フィルタ」、変数ノードは
  ;; 何でも受け入れる「フリーパス」であり、この値は面積あたりの自由度を表す。
  ;; 高いほど単一化相手が絞られず、分岐爆発の源になりやすい。
  ;;   P(x,y,u)            -> 3/3 = 1.0  (全変数節。どの P リテラルとも単一化)
  ;;   M(v, s(s(Z)), s(w)) -> 2/7 ≈ 0.29 (変数が接地構造に係留され候補が絞られる)
  ;; 「変数の絶対数」を指標にすると、大きな接地項に少数の未知数を持つ
  ;; ゴール指向の分解状態(算術系の !F(2,?0)|!M(?0,2,...) など)まで罰して
  ;; 探索が退行するため、面積で正規化している。
  (let ((size (%clause-size clause)))
    (if (zerop size)
        0
        (/ (%clause-distinct-variable-count clause) size))))


;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
; admissible な定義でないため、最短でない可能性があることに留意

(defmethod cost-to-neighbor ((node1 clause-set) (node2 clause-set))
  (let ((clauses (clause-set.clauses node2)))
    (if clauses
        (*
          ;; 節集合中の節の数
          (length clauses)
          ;; 節の長さの中央値
          (median 
            (mapcar 
              #'clause-length
              clauses))
          ;; 節の使われ具合のばらつき
          (1+ (variance
                (mapcar #'clause.used-cnt clauses)))
          ;; スカスカ（変数が多い）具合
          (1+ (* *clause-permissiveness-weight*
                 (mean (mapcar #'%clause-permissiveness clauses))))
          )
        1))) 

(defmethod cost-to-goal ((node clause-set))
  (* *heuristic-weight*              ; w（貪欲度）
     (%center-length node)           ; 残り何歩か
     (cost-to-neighbor node node)))  ; 1歩あたりのコスト ← ①をそのまま流用

;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
