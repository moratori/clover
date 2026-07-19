(defpackage clover.tests.resolution
  (:use :cl
        :clover.types
        :clover.logical-predicates
        :clover.unify
        :clover.resolution
        :1am)
  (:import-from :clover.tests.util
                :skip-test
                )
  (:import-from :clover.equality
                :clause=
                :clause-set=)
  (:import-from :clover.clover
                :start_resolution))
(in-package :clover.tests.resolution)


;;;; ==========================================================================
;;;; 監査(導出コア)で判明した「完全性ギャップ」の再現テスト
;;;;
;;;; 以下2件はいずれも RED(現行実装では FAIL する)。src/ は未修正であり、
;;;; テストを通すために実装を書き換えてはならない(役割の原則)。実装(人間側)で
;;;; 該当ギャップを解消したとき GREEN になる、という「あるべき仕様」の固定である。
;;;; ==========================================================================

(skip-test
  (test clover.tests.resolution.factoring-incompleteness
      ;; 【factoring 不在による不完全性 / 確度: 高 / 経験的に再現済み】
      ;; 事実: src/ 全体に factoring 規則が存在しない(grep 0 件)。:default 導出
      ;;   (resolution.lisp:27-80)は「解消リテラルと literal=/complement な複製の除去」
      ;;   は行うが、同符号リテラル2本を mgu で1本に併合する本来の factoring を行わない。
      ;; 反例(併せて充足不能):
      ;;   base   : {P(x), P(y)}     (≡ ∀x P(x))
      ;;   conseq : {!P(u), !P(v)}   (≡ ∀u !P(u))
      ;; 二項導出のみでは 2 リテラル節を生み続け □ に到達しない。A* は有限(重複排除済)
      ;;   状態を探索し尽くし foundp=NIL を返す(タイムアウトではない)。
      ;; factoring 追加後は foundp=T となるべき。現状この is は FAIL する。
      (multiple-value-bind (foundp node)
          (start_resolution
            (clause-set
              (list
                (clause (list (literal nil 'P (list (vterm 'x)))
                              (literal nil 'P (list (vterm 'y)))))
                (clause (list (literal t 'P (list (vterm 'u)))
                              (literal t 'P (list (vterm 'v))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp))))


#|

前提の無矛盾を前提とする

(test clover.tests.resolution.set-of-support-incompleteness
      ;; 【頂節=conseq 固定(set-of-support/線形)による不完全性 / 確度: 事実(再現済) /
      ;;   実害の有無は設計意図次第】
      ;; 事実: prepare-resolution(clover.lisp:84-114)は conseq を唯一の :center とし、
      ;;   :default opener(resolution.lisp:182-202)は :center のみを他節と導出する。
      ;;   このため反駁が conseq を経由しない場合(例: 前提節だけで不整合)、□ に到達しない。
      ;; 反例(全体は充足不能ゆえ妥当な含意):
      ;;   premises: {P}, {!P}   ← これだけで不整合
      ;;   conseq  : {!Q}        ← 反駁に不要な無関係ゴール
      ;; center=!Q は何とも導出できず foundp=NIL。
      ;; 注: 多くの証明器は「前提の無矛盾」を仮定するため、これを許容する設計もあり得る。
      ;;   その場合は本テストを「前提無矛盾を仮定するため対象外」として削除/無効化してよい。
      (multiple-value-bind (foundp node)
          (start_resolution
            (clause-set
              (list
                (clause (list (literal nil 'P nil)))
                (clause (list (literal t 'P nil)))
                (clause (list (literal t 'Q nil)) nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))
|#



(test clover.tests.resolution.resolution.test1

      (is (let* ((clause1
                   (clause 
                     (list
                       (literal nil 'P (list (vterm 'x) (vterm 'y)))
                       (literal nil 'Q (list (vterm 'x))))))
                 (clause2
                   (clause 
                     (list
                       (literal t 'P (list (vterm 'z) (vterm 'z)))
                       (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                       (literal t 'Q (list (vterm 'w))))))
                 (res 
                   (multiple-value-bind 
                       (a b resoluted)
                       (resolution clause1 clause2 :default)
                     (declare (ignore a b))
                     resoluted))
                 (expected
                   (list 
                     (clause 
                       (list 
                         (literal nil 'Q (list (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w)))))
                         (literal t 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal t 'Q (list (vterm 'w)))
                         (literal t 'P (list (vterm 'z) (vterm 'z)))
                         (literal nil 'Q (list (vterm 'w)))))
                     (clause 
                       (list 
                         (literal nil 'P (list (vterm 'w) (vterm 'y))) 
                         (literal t 'P (list (vterm 'z) (vterm 'z)))
                         (literal t 'P (list (vterm 'w) (fterm 'F (list (vterm 'w))))))))))
            (every 
              (lambda (r)
                (member r expected
                  :test #'clause=))
              res)))
  )


(test clover.tests.resolution.resolution.test2
      (let* ((clause1
               (clause (list (literal nil 'P (list (constant 'A )
                                                   (constant 'B )
                                                   (constant 'C )))))
               )
             (clause2
               (clause (list (literal nil 'P (list (vterm 'u)
                                                   (vterm 'z)
                                                   (vterm 'w)))
                             (literal t   'P (list (vterm 'y)
                                                   (vterm 'z)
                                                   (vterm 'v)))
                             (literal t   'P (list (vterm 'x)
                                                   (vterm 'y)
                                                   (vterm 'u)))
                             (literal t   'P (list (vterm 'x)
                                                   (vterm 'v)
                                                   (vterm 'w)))))
               )
             (cs
               (clause-set (list clause1 clause2) :default))
             (ret
               (clover.resolution::resolution-wrapper
                 cs
                 clause1
                 clause2
                 :default
                 (lambda (x) :center)
                 (lambda (x) :resolvent)
                 (lambda (x) :resolvent))))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'y)
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'v)
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (constant 'B )
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'x)
                                                    (constant 'A )
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (constant 'C )
                                                    (vterm 'w)))))))
            ret
            :test #'clause-set=))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'y)
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'v)
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (constant 'C )
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (constant 'B )
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (constant 'A )
                                                    (vterm 'v)
                                                    (vterm 'w)))))))
            ret
            :test #'clause-set=))

        (is 
          (member 
            (clause-set
              (list
                (clause (list (literal nil 'P (list (constant 'A )
                                                    (constant 'B )
                                                    (constant 'C )))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (vterm 'w)))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (vterm 'v)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'y)
                                                    (vterm 'u)))
                              (literal t   'P (list (vterm 'x)
                                                    (vterm 'v)
                                                    (vterm 'w)))))
                (clause (list (literal nil 'P (list (vterm 'u)
                                                    (vterm 'z)
                                                    (constant 'C )))
                              (literal t   'P (list (vterm 'y)
                                                    (vterm 'z)
                                                    (constant 'B )))
                              (literal t   'P (list (constant 'A )
                                                    (vterm 'y)
                                                    (vterm 'u)))))))
            ret
            :test #'clause-set=))))

