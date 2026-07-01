(defpackage clover.tests.rename
  (:use :cl
        :clover.types
        :clover.logical-predicates
        :clover.unify
        :clover.rename
        :1am)
  (:import-from :clover.equality
                :term=
                :clause=))
(in-package :clover.tests.rename)


(test clover.tests.rename.rename.test1

      (is 
        (let ((clause 
                (clause (list (literal t 'P   (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'Q (list (fterm 'f (list (vterm 'x) (vterm 'y)))))
                              (literal nil 'P nil)))))
          (format t "~%~A~%" (rename clause))
          t))
      
      (is 
        (let ((clause 
                (clause (list (literal t 'P   (list (vterm 'x) (vterm 'y)))
                              (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'Q (list (fterm 'f (list (vterm 'x) (vterm 'y)))))
                              (literal nil 'P nil)))))
          (format t "~%~A~%" (rename clause))
          t))
      
      (is 
        (let*((clause1
                (clause (list (literal t 'P   (list (vterm 'x) (vterm 'y)))
                              (literal nil 'P (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'Q (list (fterm 'f (list (vterm 'x) (vterm 'y)))))
                              (literal nil 'P nil))))
              (clause2
                (clause (list (literal t 'P   (list (vterm 'x) (vterm 'y)))
                              (literal nil 'Q (list (fterm 'f (list (vterm 'x) (vterm 'y))))))))
              (clause-set
                (clause-set (list clause1 clause2))))
          (format t "~%~A~%" (rename clause-set))
          t))

      )

(test clover.tests.rename.rename-for-human-readable.test1

      (let ((clause
              (clause 
                  (list (literal t 'P   (list (vterm 'hoge) 
                                              (constant 'foo ) 
                                              (fterm 'bar (list (vterm 'x))))))))
            (expected
              (clause 
                  (list (literal t 'P   (list (vterm 'clover.parser::x) 
                                              (constant 'foo ) 
                                              (fterm 'bar (list (vterm 'clover.parser::y)))))))))

        (is (clause= (rename-for-human-readable-printing clause)
                     expected))))



(test clover.tests.rename.rename.injective-and-alpha-equiv
      ;; gensym 版 rename は単射・同一性保存・アルファ同値を保つ（正常動作の回帰ガード）。
      ;; 既存の rename.test1 はリネーム結果を検証せずリテラル t を返すだけだったため、
      ;; 横展開調査で判明した「検証の欠落」を補う。
      ;; f(x,x): 両出現が同一の新変数になる（同一性保存）
      (let* ((cl (clause (list (literal nil 'P (list (fterm 'f (list (vterm 'x) (vterm 'x))))))))
             (r  (rename cl))
             (vars (remove-duplicates (mapcan #'collect-variables (clause.literals r)) :test #'term=)))
        (is (= 1 (length vars))))
      ;; f(x,y): 異なる変数は異なる新変数になる（単射）
      (let* ((cl (clause (list (literal nil 'P (list (vterm 'x) (vterm 'y))))))
             (r  (rename cl))
             (vars (remove-duplicates (mapcan #'collect-variables (clause.literals r)) :test #'term=)))
        (is (= 2 (length vars))))
      ;; リネーム結果は元とアルファ同値
      (let ((cl (clause (list (literal nil 'P (list (vterm 'x) (vterm 'y)))
                              (literal t 'Q (list (vterm 'y)))))))
        (is (alphabet-equivalent-p cl (rename cl)))))

(test clover.tests.rename.human-readable.injective
      ;; 横展開調査で判明した不具合: rename-for-human-readable-printing が
      ;; 写像を apply-unifier-set の逐次置換で適用するため、ソース変数名が
      ;; 固定名(X,Y,...)と衝突する順序で並ぶと変数捕獲(variable capture)を起こし非単射。
      ;; P(Y,X)（変数収集順 [Y,X] → 割当 [Y->X, X->Y]）は p(y,y) に潰れる。
      ;; 表示専用関数で証明の健全性には非影響だが、相異なる変数が同一表示になる誤り。
      ;; → 下記の1つ目は現実装では FAIL する。
      (flet ((psym (s) (clover.parser::%intern-symbol-to-specified-package s)))
        ;; 衝突順: 相異なる2変数が保たれるべき（本来 2）
        (let* ((cl (clause (list (literal nil 'P (list (vterm (psym "Y")) (vterm (psym "X")))))))
               (r  (rename-for-human-readable-printing cl))
               (vars (remove-duplicates (mapcan #'collect-variables (clause.literals r)) :test #'term=)))
          (is (= 2 (length vars))))
        ;; 対照: 非衝突順 P(X,Y) は正常（2変数）
        (let* ((cl (clause (list (literal nil 'P (list (vterm (psym "X")) (vterm (psym "Y")))))))
               (r  (rename-for-human-readable-printing cl))
               (vars (remove-duplicates (mapcan #'collect-variables (clause.literals r)) :test #'term=)))
          (is (= 2 (length vars))))))
