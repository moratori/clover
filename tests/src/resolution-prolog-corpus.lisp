;;;; Prolog 入門・応用の定番問題による導出(反駁)コーパステスト
;;;;
;;;; 出典 (source):
;;;;   Prolog 入門サイト・教科書の定番例題を、clover の入力モデル
;;;;   (Horn節 + 単一 :conseq 節 = set-of-support) へ手翻訳したもの。
;;;;   各テストのコメントに個別の出典を記す。
;;;;
;;;; 変換方針:
;;;;   - Prolog の規則 head :- body は節 {head, ¬body...} に、事実は単位節に、
;;;;     問い合わせ ?- goal は :conseq 節 {¬goal} に対応させる。
;;;;   - カット(!)・negation as failure・対話的 verify は使えないため、
;;;;     それらを含む原典は純粋な Horn 規則+事実に純化して翻訳する。
;;;;   - 空リストは CL:NIL との紛れ(シンボル NIL の判定バグの温床)を避けて
;;;;     定数 EMPTY で表す。
;;;;   - 命名は既存規約に従い、定数=大文字・変数/関数記号=小文字で書く。

(defpackage clover.tests.resolution.prolog-corpus
  (:use :cl
        :clover.types
        :1am)
  (:import-from :clover.clover
                :start-resolution))
(in-package :clover.tests.resolution.prolog-corpus)


(test clover.tests.resolution.prolog-corpus.family-ancestor-chain
      ;; 家系図と祖先関係(ancestor の推移閉包)。Prolog 入門の最初の定番。
      ;; 出典: TutorialsPoint "Prolog - Relations" / 101computing "Prolog Family Tree"
      ;;   parent(tom,bob). parent(bob,ann). parent(ann,pat).
      ;;   ancestor(X,Y) :- parent(X,Y).
      ;;   ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).
      ;;   ?- ancestor(tom,pat).   (3世代をまたぐ再帰)
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'parent (list (constant 'TOM) (constant 'BOB)))))
                (clause (list (literal nil 'parent (list (constant 'BOB) (constant 'ANN)))))
                (clause (list (literal nil 'parent (list (constant 'ANN) (constant 'PAT)))))
                (clause (list (literal t   'parent (list (vterm 'x) (vterm 'y)))
                              (literal nil 'ancestor (list (vterm 'x) (vterm 'y)))))
                (clause (list (literal t   'parent (list (vterm 'x) (vterm 'y)))
                              (literal t   'ancestor (list (vterm 'y) (vterm 'z)))
                              (literal nil 'ancestor (list (vterm 'x) (vterm 'z)))))
                (clause (list (literal t 'ancestor (list (constant 'TOM) (constant 'PAT))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.family-grandparent-unprovable
      ;; 証明できない問い合わせが foundp=nil で「停止して」返ること(有限探索)の確認。
      ;; 注意: ancestor のような再帰規則を含む理論では、証明不能な問い合わせは
      ;; 再帰規則の頭部との導出が変数を残したまま無限に続き停止しない
      ;; (SLD の左端選択と違いリテラル選択が自由なため、解決不能なリテラルが
      ;;  残っていても再帰側を展開し続けられる)。そのため否定側のテストは
      ;; 再帰規則を含まない問題に限定する。ここでは葉にあたる ann には孫が
      ;; いないため、非再帰の grandparent 規則だけで探索が有限に閉じる。
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'father (list (constant 'TOM) (constant 'BOB)))))
                (clause (list (literal nil 'father (list (constant 'BOB) (constant 'ANN)))))
                (clause (list (literal t   'father (list (vterm 'x) (vterm 'y)))
                              (literal nil 'parent (list (vterm 'x) (vterm 'y)))))
                (clause (list (literal t   'parent (list (vterm 'x) (vterm 'y)))
                              (literal t   'parent (list (vterm 'y) (vterm 'z)))
                              (literal nil 'grandparent (list (vterm 'x) (vterm 'z)))))
                (clause (list (literal t 'grandparent (list (constant 'ANN) (vterm 'w))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is (not foundp))))


(test clover.tests.resolution.prolog-corpus.family-grandparent-query-variable
      ;; father/mother から parent を定義し grandparent を問う。変数付き問い合わせ
      ;; ?- grandparent(tom, W) (答え W=ann の存在証明)。
      ;; 出典: 101computing "Prolog Family Tree" / EDUCBA "Prolog Family Tree"
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'father (list (constant 'TOM) (constant 'BOB)))))
                (clause (list (literal nil 'mother (list (constant 'PAM) (constant 'BOB)))))
                (clause (list (literal nil 'father (list (constant 'BOB) (constant 'ANN)))))
                (clause (list (literal t   'father (list (vterm 'x) (vterm 'y)))
                              (literal nil 'parent (list (vterm 'x) (vterm 'y)))))
                (clause (list (literal t   'mother (list (vterm 'x) (vterm 'y)))
                              (literal nil 'parent (list (vterm 'x) (vterm 'y)))))
                (clause (list (literal t   'parent (list (vterm 'x) (vterm 'y)))
                              (literal t   'parent (list (vterm 'y) (vterm 'z)))
                              (literal nil 'grandparent (list (vterm 'x) (vterm 'z)))))
                (clause (list (literal t 'grandparent (list (constant 'TOM) (vterm 'w))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.list-member
      ;; リストの member/2。リスト再帰の定番第1問。
      ;; 出典: Anniepoo/prolog-examples ほか入門書全般
      ;;   member(X, [X|_]).  member(X, [_|T]) :- member(X, T).
      ;;   ?- member(c, [a,b,c]).   (末尾要素の発見 = 再帰2段)
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'member
                                        (list (vterm 'x)
                                              (fterm 'cons (list (vterm 'x) (vterm 't)))))))
                (clause (list (literal t   'member (list (vterm 'x) (vterm 't)))
                              (literal nil 'member
                                       (list (vterm 'x)
                                             (fterm 'cons (list (vterm 'h) (vterm 't)))))))
                (clause (list (literal t 'member
                                       (list (constant 'C)
                                             (fterm 'cons
                                                    (list (constant 'A)
                                                          (fterm 'cons
                                                                 (list (constant 'B)
                                                                       (fterm 'cons
                                                                              (list (constant 'C)
                                                                                    (constant 'EMPTY))))))))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.list-append-variable-result
      ;; リストの append/3。結果を変数で受ける問い合わせ。
      ;; 出典: Anniepoo/prolog-examples / 入門書全般
      ;;   append([], Y, Y).  append([H|T], Y, [H|Z]) :- append(T, Y, Z).
      ;;   ?- append([a,b], [c], W).   (W = [a,b,c] の存在証明)
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'append
                                        (list (constant 'EMPTY) (vterm 'y) (vterm 'y)))))
                (clause (list (literal t   'append
                                        (list (vterm 't) (vterm 'y) (vterm 'z)))
                              (literal nil 'append
                                       (list (fterm 'cons (list (vterm 'h) (vterm 't)))
                                             (vterm 'y)
                                             (fterm 'cons (list (vterm 'h) (vterm 'z)))))))
                (clause (list (literal t 'append
                                       (list (fterm 'cons
                                                    (list (constant 'A)
                                                          (fterm 'cons
                                                                 (list (constant 'B)
                                                                       (constant 'EMPTY)))))
                                             (fterm 'cons (list (constant 'C) (constant 'EMPTY)))
                                             (vterm 'w))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.list-last
      ;; リストの last/2。単一要素の基底節と再帰の組合せ。
      ;; 出典: お気楽 Prolog プログラミング入門 (M.Hiroi) のリスト操作例題ほか
      ;;   last([X], X).  last([_|T], X) :- last(T, X).
      ;;   ?- last([a,b,c], W).
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'last
                                        (list (fterm 'cons (list (vterm 'x) (constant 'EMPTY)))
                                              (vterm 'x)))))
                (clause (list (literal t   'last (list (vterm 't) (vterm 'x)))
                              (literal nil 'last
                                       (list (fterm 'cons (list (vterm 'h) (vterm 't)))
                                             (vterm 'x)))))
                (clause (list (literal t 'last
                                       (list (fterm 'cons
                                                    (list (constant 'A)
                                                          (fterm 'cons
                                                                 (list (constant 'B)
                                                                       (fterm 'cons
                                                                              (list (constant 'C)
                                                                                    (constant 'EMPTY)))))))
                                             (vterm 'w))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.peano-addition
      ;; ペアノ算術の加算 add/3 (後者関数 s による再帰)。2+3=W の存在証明。
      ;; 出典: 田村直之「Prolog 入門」講義資料 / Wikibooks Prolog ほか定番
      ;;   add(zero, Y, Y).  add(s(X), Y, s(Z)) :- add(X, Y, Z).
      ;;   ?- add(s(s(zero)), s(s(s(zero))), W).
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'add
                                        (list (constant 'ZERO) (vterm 'y) (vterm 'y)))))
                (clause (list (literal t   'add (list (vterm 'x) (vterm 'y) (vterm 'z)))
                              (literal nil 'add
                                       (list (fterm 's (list (vterm 'x)))
                                             (vterm 'y)
                                             (fterm 's (list (vterm 'z)))))))
                (clause (list (literal t 'add
                                       (list (fterm 's (list (fterm 's (list (constant 'ZERO)))))
                                             (fterm 's (list (fterm 's (list (fterm 's (list (constant 'ZERO)))))))
                                             (vterm 'w))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.graph-path-dag
      ;; 有向非巡回グラフの到達可能性 path/2。経路が複数ある DAG での探索。
      ;; 出典: Prolog Guide (Charles Univ.) の genealogy と同型の教科書的例題
      ;;   edge(a,b). edge(b,c). edge(a,d). edge(d,c). edge(c,e).
      ;;   path(X,Y) :- edge(X,Y).  path(X,Z) :- edge(X,Y), path(Y,Z).
      ;;   ?- path(a,e).
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'edge (list (constant 'A) (constant 'B)))))
                (clause (list (literal nil 'edge (list (constant 'B) (constant 'C)))))
                (clause (list (literal nil 'edge (list (constant 'A) (constant 'D)))))
                (clause (list (literal nil 'edge (list (constant 'D) (constant 'C)))))
                (clause (list (literal nil 'edge (list (constant 'C) (constant 'E)))))
                (clause (list (literal t   'edge (list (vterm 'x) (vterm 'y)))
                              (literal nil 'path (list (vterm 'x) (vterm 'y)))))
                (clause (list (literal t   'edge (list (vterm 'x) (vterm 'y)))
                              (literal t   'path (list (vterm 'y) (vterm 'z)))
                              (literal nil 'path (list (vterm 'x) (vterm 'z)))))
                (clause (list (literal t 'path (list (constant 'A) (constant 'E))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.animal-expert-system-tiger
      ;; 動物判定エキスパートシステム(応用事例の定番)。
      ;; 出典: J.R.Fisher "Prolog Tutorial" 2.17 Animal identification game
      ;;   原典は対話的 verify とカットを使うため、個体 STRIPY についての
      ;;   観察事実 + 純粋な Horn 分類規則へ純化して翻訳した。
      ;;   mammal(X) :- has_hair(X).
      ;;   carnivore(X) :- mammal(X), eats_meat(X).
      ;;   tiger(X) :- mammal(X), carnivore(X), has_tawny_color(X), has_black_stripes(X).
      ;;   ?- tiger(stripy).
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                (clause (list (literal nil 'has_hair (list (constant 'STRIPY)))))
                (clause (list (literal nil 'eats_meat (list (constant 'STRIPY)))))
                (clause (list (literal nil 'has_tawny_color (list (constant 'STRIPY)))))
                (clause (list (literal nil 'has_black_stripes (list (constant 'STRIPY)))))
                (clause (list (literal t   'has_hair (list (vterm 'x)))
                              (literal nil 'mammal (list (vterm 'x)))))
                (clause (list (literal t   'mammal (list (vterm 'x)))
                              (literal t   'eats_meat (list (vterm 'x)))
                              (literal nil 'carnivore (list (vterm 'x)))))
                (clause (list (literal t   'mammal (list (vterm 'x)))
                              (literal t   'carnivore (list (vterm 'x)))
                              (literal t   'has_tawny_color (list (vterm 'x)))
                              (literal t   'has_black_stripes (list (vterm 'x)))
                              (literal nil 'tiger (list (vterm 'x)))))
                (clause (list (literal t 'tiger (list (constant 'STRIPY))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))


(test clover.tests.resolution.prolog-corpus.monkey-banana
      ;; モンキーバナナ問題(プランニングの古典)。
      ;; 出典: Bratko "Prolog Programming for AI" 2.5 (people.uncw.edu/narayans の
      ;;   講義資料 monkey.pdf) / TutorialsPoint "Monkey and Banana Problem"
      ;;   状態 state(サルの水平位置, サルの垂直位置, 箱の位置, バナナ所持) と
      ;;   4種の move (grasp/climb/push/walk)、canget の再帰で表現する。
      ;;   ?- canget(state(atdoor, onfloor, atwindow, hasnot)).
      ;;   解は walk -> push -> climb -> grasp の4手。
      (multiple-value-bind (foundp node)
          (start-resolution
            (clause-set
              (list
                ;; move(state(middle,onbox,middle,hasnot), grasp, state(middle,onbox,middle,has)).
                (clause (list (literal nil 'move
                                        (list (fterm 'state (list (constant 'MIDDLE) (constant 'ONBOX)
                                                                  (constant 'MIDDLE) (constant 'HASNOT)))
                                              (constant 'GRASP)
                                              (fterm 'state (list (constant 'MIDDLE) (constant 'ONBOX)
                                                                  (constant 'MIDDLE) (constant 'HAS)))))))
                ;; move(state(P,onfloor,P,H), climb, state(P,onbox,P,H)).
                (clause (list (literal nil 'move
                                        (list (fterm 'state (list (vterm 'p) (constant 'ONFLOOR)
                                                                  (vterm 'p) (vterm 'h)))
                                              (constant 'CLIMB)
                                              (fterm 'state (list (vterm 'p) (constant 'ONBOX)
                                                                  (vterm 'p) (vterm 'h)))))))
                ;; move(state(P1,onfloor,P1,H), push, state(P2,onfloor,P2,H)).
                (clause (list (literal nil 'move
                                        (list (fterm 'state (list (vterm 'p1) (constant 'ONFLOOR)
                                                                  (vterm 'p1) (vterm 'h)))
                                              (constant 'PUSH)
                                              (fterm 'state (list (vterm 'p2) (constant 'ONFLOOR)
                                                                  (vterm 'p2) (vterm 'h)))))))
                ;; move(state(P1,onfloor,B,H), walk, state(P2,onfloor,B,H)).
                (clause (list (literal nil 'move
                                        (list (fterm 'state (list (vterm 'p1) (constant 'ONFLOOR)
                                                                  (vterm 'b) (vterm 'h)))
                                              (constant 'WALK)
                                              (fterm 'state (list (vterm 'p2) (constant 'ONFLOOR)
                                                                  (vterm 'b) (vterm 'h)))))))
                ;; canget(state(_,_,_,has)).
                (clause (list (literal nil 'canget
                                        (list (fterm 'state (list (vterm 'mp) (vterm 'mv)
                                                                  (vterm 'bp) (constant 'HAS)))))))
                ;; canget(S1) :- move(S1, M, S2), canget(S2).
                (clause (list (literal t   'move (list (vterm 's1) (vterm 'm) (vterm 's2)))
                              (literal t   'canget (list (vterm 's2)))
                              (literal nil 'canget (list (vterm 's1)))))
                ;; ?- canget(state(atdoor,onfloor,atwindow,hasnot)).
                (clause (list (literal t 'canget
                                       (list (fterm 'state (list (constant 'ATDOOR) (constant 'ONFLOOR)
                                                                 (constant 'ATWINDOW) (constant 'HASNOT))))))
                        nil nil nil :conseq))))
        (declare (ignore node))
        (is foundp)))
