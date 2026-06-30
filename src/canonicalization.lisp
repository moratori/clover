;;;; clover.canonicalization
;;;;
;;;; 項・リテラル・節・等式・書き換え規則を「変数リネーム不変」な正準キー文字列へ
;;;; 直列化するモジュール。α同値(変数の全単射による変種)な対象を同一キーへ畳むことで、
;;;; ハッシュによる高速な重複排除や、探索の closed 集合の同一視に用いる。
;;;;
;;;; 設計方針:
;;;;  - 依存は clover.types のみ(低レイヤの独立モジュール)。
;;;;  - キーは over-approximation: alphabet=(x,y) が真なら必ず同一キーになる。
;;;;    (逆は成り立たなくてよい。同一キーのバケツ内で従来述語により最終確認する運用)
;;;;  - 1パスの文字列直列化のみで、rename 等のオブジェクト再構築を行わず軽量。
;;;;
;;;; 命名規約(-string と -key の役割差):
;;;;  - `*-string`  … 直列化の「部品」。変数リネーム以外のドメイン固有の対称性は畳まず、
;;;;                   単体ではキーとして完結しない。
;;;;                     canonical-term-string   : 共有ストリームへ書き込むプリミティブ
;;;;                     canonical-clause-string : 一節分の断片(節集合キーは clover.clover の
;;;;                                               node-canonical-key が各節文字列をソート+連結して作る)
;;;;  - `*-key`     … その型の同値性まで畳み込んだ「単体で完結する正準キー」。
;;;;                   remove-duplicates-by-key の key-fn にそのまま渡せる。
;;;;                     canonical-equation-key       : 左右入替の対称性+否定を畳む
;;;;                     canonical-rewrite-rule-key   : 向きは保持

(defpackage clover.canonicalization
  (:use :cl
        :clover.types)
  (:export
    :canonical-term-string
    :canonical-clause-string
    :canonical-equation-key
    :canonical-rewrite-rule-key
    ))
(in-package :clover.canonicalization)


(defun canonical-term-string (term var-index counter-cell out)
  "[部品/-string] term を out へ直列化する低レベルプリミティブ。返り値ではなく out への副作用が結果。
   変数は var-index(変数シンボル->番号) に従い初出順で番号付け。
   constant は fterm のサブタイプのため fterm より先に分岐する。
   var-index/counter-cell を複数の項で共有して呼べば、共有変数を同一番号に揃えられる。
   それ自体は単体のキーではなく、上位の *-string/*-key から組み立て部品として呼ばれる。"
  (typecase term
    (vterm
     (let ((idx (or (gethash (vterm.var term) var-index)
                    (setf (gethash (vterm.var term) var-index)
                          (prog1 (car counter-cell) (incf (car counter-cell)))))))
       (write-char #\? out)
       (princ idx out)))
    (constant
     (write-char #\# out)
     (write-string (symbol-name (constant.value term)) out))
    (fterm
     (write-string (symbol-name (fterm.fsymbol term)) out)
     (write-char #\( out)
     (loop :for a :in (fterm.args term)
           :for firstp := t :then nil
           :do (unless firstp (write-char #\, out))
               (canonical-term-string a var-index counter-cell out))
     (write-char #\) out))
    (t (princ term out))))

(defun %canonical-term-pair-string (a b)
  "[部品/-string] 2項 (a, b) を共有変数番号で1本の文字列に直列化する(変数リネーム不変)。
   -key 関数(equation/rewrite-rule)が向き付けを与えて呼ぶ内部ヘルパ。"
  (let ((var-index (make-hash-table :test #'eq))
        (counter-cell (list 0))
        (out (make-string-output-stream)))
    (canonical-term-string a var-index counter-cell out)
    (write-char #\= out)
    (canonical-term-string b var-index counter-cell out)
    (get-output-stream-string out)))

(defun canonical-clause-string (clause)
  "[部品/-string] 節を変数リネーム不変な文字列へ1パスで直列化する(rename等のオブジェクト再構築を伴わず軽量)。
   変数は節内の初出順に番号付け(節集合の alphabet= は節ごと独立の変数全単射のため節ローカルで正しい)。
   リテラルは clause.literals の順序のまま(リテラル内/順序や condensation までは畳まない)。
   これは「一節分の断片」であり単体では節集合のキーにならない。節集合のキーは clover.clover の
   node-canonical-key が各節の本文字列をソート+連結して作る(節順非依存に畳むのはそちら)。"
  (let ((var-index (make-hash-table :test #'eq))
        (counter-cell (list 0))
        (out (make-string-output-stream)))
    (loop :for lit :in (clause.literals clause)
          :for firstp := t :then nil
          :do
          (unless firstp (write-char #\| out))
          (when (literal.negation lit) (write-char #\! out))
          (write-string (symbol-name (literal.predicate lit)) out)
          (write-char #\( out)
          (loop :for a :in (literal.args lit)
                :for f := t :then nil
                :do (unless f (write-char #\, out))
                    (canonical-term-string a var-index counter-cell out))
          (write-char #\) out))
    (get-output-stream-string out)))

(defun canonical-equation-key (equation)
  "[完結キー/-key] 等式のα同値正準キー。左右入替不変(2向きのうち小さい方)・変数リネーム不変・否定込み。
   単体で remove-duplicates-by-key の key-fn にそのまま渡せる。
   alphabet=(eq1,eq2) が真なら必ず同一キーになる(over-approximation)。"
  (let ((s1 (%canonical-term-pair-string (equation.left equation)
                                         (equation.right equation)))
        (s2 (%canonical-term-pair-string (equation.right equation)
                                         (equation.left equation))))
    (concatenate 'string
                 (if (equation.negation equation) "!" "")
                 (if (string<= s1 s2) s1 s2))))

(defun canonical-rewrite-rule-key (rewrite-rule)
  "[完結キー/-key] 書き換え規則のα同値正準キー。src->dst の向きは保持、変数リネーム不変。
   単体で remove-duplicates-by-key の key-fn にそのまま渡せる。"
  (%canonical-term-pair-string (rewrite-rule.src rewrite-rule)
                               (rewrite-rule.dst rewrite-rule)))
