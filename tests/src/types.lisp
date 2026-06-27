(defpackage clover.tests.types
  (:use :cl
        :clover.types
        :1am))
(in-package :clover.tests.types)


(test clover.tests.types.vterm 
      (is (vterm 'x))
      (is (vterm 'x_y_z-a-b-c))
      )

(test clover.tests.types.fterm
      (is (fterm 'f (list (vterm 'x) (vterm 'y))))
      (is (fterm 'g (list (vterm 'x) (fterm 'h (list (vterm 'z))))))
      (is (constant 'h ))
      )


(test clover.tests.types.fterm.zero-args-is-constant
      ;; fterm コンストラクタは引数が空(nil)のとき constant を返す。
      ;; すなわち 0 引数の fterm は生成されず、必ず constant に正規化される。
      ;; （src/types.lisp の fterm ラッパ:
      ;;   (if (null args) (constant fsymbol) (%fterm fsymbol args)) ）
      (let ((z (fterm 'B nil)))
        ;; constant 型である（constant は fterm のサブタイプなので fterm でもある）。
        (is (typep z 'constant))
        (is (typep z 'fterm))
        ;; スロットの値が期待どおり。
        (is (eq 'B (fterm.fsymbol z)))
        (is (null (fterm.args z)))
        (is (eq 'B (constant.value z)))
        ;; constant コンストラクタ経由と同じ型になる。
        (is (typep (constant 'B) 'constant)))

      ;; 引数が非空なら従来どおり通常の fterm（constant ではない）。
      (let ((f (fterm 'f (list (vterm 'x)))))
        (is (typep f 'fterm))
        (is (not (typep f 'constant)))
        (is (eq 'f (fterm.fsymbol f)))
        (is (= 1 (length (fterm.args f))))
        (is (typep (first (fterm.args f)) 'vterm))))

(test clover.tests.types.literal
        (is (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))
        (is (literal t 'P   (list (vterm 'x) (vterm 'y) (vterm 'z))))
        (is (literal nil 'P (list (vterm 'x))))
        (is (literal t 'P (list (vterm 'x))))
        (is (literal nil 'P nil))
        (is (literal t 'P nil))
        )

(test clover.tests.types.clause
        (is (clause nil))
        (is (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z))))))
        (is (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'P nil))))
        (is (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                            (literal nil 'Pred3 nil))))
        )

(test clover.tests.types.clause-set
        (is (clause-set (list (clause nil) 
                              (clause (list (literal t 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))))
                              (clause (list (literal t 'Pred1 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred2 (list (vterm 'x) (vterm 'y) (vterm 'z)))
                                              (literal nil 'Pred3 nil))))))
        )
 
