(defpackage clover.rewrite
  (:use :cl
        :clover.property
        :clover.conditions
        :clover.types 
        :clover.util
        :clover.unify
        :clover.substitute
        :clover.rename
        )
  (:export
    :apply-rewrite-rule
    :apply-rewrite-rule-set
    :rewrite-final
    )
  )
(in-package :clover.rewrite)



(defmethod %rewrite ((vterm vterm) (src vterm) (dst vterm))
  vterm)

(defmethod %rewrite ((vterm vterm) (src vterm) (dst fterm))
  (apply-unifier
    dst
    (unifier src vterm)))

(defmethod %rewrite ((vterm vterm) (src fterm) (dst term))
  vterm)


(defmethod %rewrite ((fterm fterm) (src vterm) (dst term))
  (apply-unifier
    dst
    (unifier src fterm)))


(defmethod %rewrite ((fterm fterm) (src fterm) (dst term))
  (let* ((fsymbol1 (fterm.fsymbol fterm))
         (args1    (fterm.args fterm))
         (arity1   (length args1))
         (fsymbol2 (fterm.fsymbol src))
         (args2    (fterm.args src))
         (arity2   (length args2))
         (rewrited-args (mapcar (lambda (x) (%rewrite x src dst)) args1))
         (new (fterm fsymbol1 rewrited-args)))
    (handler-case
        (let* ((unifset
                 (find-most-general-unifier-set new src))
               (new-variables
                 (collect-variables new))
               ;; 対象項(fterm)中のvtermを元とする、unifierを削除する
               ;; find-most-general-unifier-set は、元々導出の為の実装であるため
               ;; 項A と 項B中のどちらのvtermを元とするunifierがあり得る
               ;; 以下のようなケースで、xをNILに書き換えてしまうことを防ぐ必要がある
               ;; fterm : reverse(x)
               ;; rule  : reverse(NIL) -> NIL  
               (corrected-unifset
                 (unifier-set
                   (remove-if
                     (lambda (u)
                       (find-if 
                         (lambda (n)
                           (term= (unifier.src u) n)) 
                         new-variables))
                     (unifier-set.unifiers unifset))))
               (null-by-erased 
                 (and (unifier-set.unifiers unifset)
                      (null (unifier-set.unifiers corrected-unifset)))))
          (cond 
            ((and (eq fsymbol1 fsymbol2)
                   (= arity1 arity2)
                   null-by-erased)
             new)
            ((and (eq fsymbol1 fsymbol2)
                   (= arity1 arity2))
             (apply-unifier-set dst corrected-unifset))
            (t new)))
        (ununifiable-error (c) new))))


(defmethod apply-rewrite-rule ((term term) (rewrite-rule rewrite-rule))
  (let* ((rule (rename rewrite-rule))
         (src (rewrite-rule.src rule))
         (dst (rewrite-rule.dst rule)))
    (%rewrite term src dst)))

(defmethod apply-rewrite-rule-set ((term term) (rewrite-rule-set rewrite-rule-set))
  (reduce 
    #'apply-rewrite-rule
    (rewrite-rule-set.rewrite-rules rewrite-rule-set)
    :initial-value term))


(defmethod rewrite-final ((term term) (rewrite-rule-set rewrite-rule-set))
  (let ((applied (apply-rewrite-rule-set term rewrite-rule-set)))
    (if (term= applied term)
        applied
        (rewrite-final applied rewrite-rule-set))))

