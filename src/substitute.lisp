(defpackage clover.substitute
  (:use :cl
        :clover.conditions
        :clover.types
        :clover.logical-predicates
        )
  (:import-from :clover.equality
                :term=)
  (:export
    :apply-unifier
    :apply-unifier-set
    )
  )
(in-package :clover.substitute)



(defmethod apply-unifier ((vterm vterm) (unifier unifier))
  (let* ((src (unifier.src unifier))
         (dst (unifier.dst unifier))
         (orgstr-vterm (vterm.original-str vterm))
         (orgstr-src   (vterm.original-str src)))
    (cond
      ((string/= orgstr-src "")
       (if (and (term= src vterm)
                (string= orgstr-src orgstr-vterm))
           dst
           vterm))
      (t
       (if (term= src vterm)
           dst
           vterm)))))

(defmethod apply-unifier ((constant constant) (unifier unifier))
  (if (term= constant (unifier.src unifier))
      (unifier.dst unifier)
      constant))

(defmethod apply-unifier ((fterm fterm) (unifier unifier))
  (fterm
    (fterm.fsymbol fterm)
    (mapcar 
      (lambda (arg)
        (apply-unifier arg unifier))
      (fterm.args fterm))))

(defmethod apply-unifier ((literal literal) (unifier unifier))
  (literal
    (literal.negation literal)
    (literal.predicate literal)
    (mapcar 
      (lambda (arg)
        (apply-unifier arg unifier))
      (literal.args literal))))

(defmethod apply-unifier ((clause clause) (unifier unifier))
  (clause 
    (mapcar
      (lambda (literal)
        (apply-unifier literal unifier))
      (clause.literals clause))
    (clause.parent1 clause)
    (clause.parent2 clause)
    (clause.unifier clause)
    (clause.clause-type clause)
    (clause.used-cnt clause)))

(defmethod apply-unifier ((equation equation) (unifier unifier))
  (equation
    (equation.negation equation)
    (apply-unifier (equation.left equation) unifier)
    (apply-unifier (equation.right equation) unifier)))

(defmethod apply-unifier ((target unifier) (unifier unifier))
  (let ((src (apply-unifier (unifier.src target) unifier))
        (dst (apply-unifier (unifier.dst target) unifier)))
    (when (typep src 'fterm)
      (error
        (make-condition 'unexpected-unifier-source
                        :message "source of unifier is fterm"
                        :src src)))
    (unifier src dst)))



;; apply-unifier-set は unifier-set 全体を「1回の走査」でまとめて適用する。
;; 素朴に reduce #'apply-unifier すると unifier の個数 k 回だけ項/節の全体を
;; 走査・再構築してしまい O(k*n) になる。そこで apply-unifier と同じく構造体ごとの
;; defmethod に分割し、構造ノード(fterm/literal/clause/equation)は1回だけ再構築、
;; 葉(vterm/constant)で全 unifier を逐次適用する。
;; 葉で (reduce #'apply-unifier ...) を行うため、unifier を左から逐次適用する
;; チェーン代入の意味論({x:=y, y:=A} を x へ → A)は従来と一致する。

(defmethod apply-unifier-set ((vterm vterm) (unifier-set unifier-set))
  (reduce
    #'apply-unifier
    (unifier-set.unifiers unifier-set)
    :initial-value vterm))

(defmethod apply-unifier-set ((constant constant) (unifier-set unifier-set))
  ;; constant は fterm のサブタイプなので、明示メソッドが無いと下の fterm メソッドに
  ;; 吸われ unifier を無視してしまう。葉として逐次適用する。
  (reduce
    #'apply-unifier
    (unifier-set.unifiers unifier-set)
    :initial-value constant))

(defmethod apply-unifier-set ((fterm fterm) (unifier-set unifier-set))
  (fterm
    (fterm.fsymbol fterm)
    (mapcar
      (lambda (arg)
        (apply-unifier-set arg unifier-set))
      (fterm.args fterm))))

(defmethod apply-unifier-set ((literal literal) (unifier-set unifier-set))
  (literal
    (literal.negation literal)
    (literal.predicate literal)
    (mapcar
      (lambda (arg)
        (apply-unifier-set arg unifier-set))
      (literal.args literal))))

(defmethod apply-unifier-set ((clause clause) (unifier-set unifier-set))
  (clause
    (mapcar
      (lambda (literal)
        (apply-unifier-set literal unifier-set))
      (clause.literals clause))
    (clause.parent1 clause)
    (clause.parent2 clause)
    (clause.unifier clause)
    (clause.clause-type clause)
    (clause.used-cnt clause)))

(defmethod apply-unifier-set ((equation equation) (unifier-set unifier-set))
  ;; equation は literal のサブタイプ。明示メソッドが無いと literal メソッド(args 走査)に
  ;; 吸われるため、left/right を再帰する専用メソッドが必要。
  (equation
    (equation.negation equation)
    (apply-unifier-set (equation.left equation) unifier-set)
    (apply-unifier-set (equation.right equation) unifier-set)))

;; フォールバック: 上記の具体型に該当しない term / logical-expression のための従来実装。
;; 現行の呼び出しは全て上の具体メソッドでカバーされるが、後方互換・防御として残す。
(defmethod apply-unifier-set ((term term) (unifier-set unifier-set))
  (reduce
    #'apply-unifier
    (unifier-set.unifiers unifier-set)
    :initial-value term))

(defmethod apply-unifier-set ((logical-expression logical-expression) (unifier-set unifier-set))
  (reduce
    #'apply-unifier
    (unifier-set.unifiers unifier-set)
    :initial-value logical-expression))

