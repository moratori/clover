# Clover

[![Build Status](https://travis-ci.org/moratori/clover.svg?branch=master)](https://travis-ci.org/moratori/clover)

## 概要

一階述語論理式の証明を行うCommon Lisp実装のツールです。以下のアルゴリズムを実装しています。
* 導出原理(Resolution Principle)
* Knuth-Bendixの完備化アルゴリズム

なお、導出原理を用いて式の証明ができた場合は、導出反駁木としてGraphvizのコードを出力することが可能です。

![sample proof figure](sample-refutation-tree.png)

## 使い方

`:def-axiom`コマンドを用いて、証明したい式の前提となる式を定義します。
その後、証明したい式を入力することで証明を試行します。

### 導出原理による証明例

```
$ ./roswell/clover.ros
Command help
    :help                    show this help
    :quit                    quit from REPL
    :def-axiom    <name>     define an axiomatic system <name>
    :show-axiom              enumerate all axiomatic system that are currently defined
    :set-axiom    <name>     set current axiomatic system to <name>
    :set-history             keep resolution history. this option automatically
                             enabled if save-tree option on.
    :unset-history           disable history
    :set-profiler            enable statistical profiler
    :unset-profiler          disable statistical profiler
    :save-tree    <path>     save Graphviz code tree to <path>
    :unsave-tree
(NIL)>>> :def-axiom human
input . to finish definition
axiom[1]>>> !love(x,y) | !love(y,x) | happy(y)
axiom[2]>>> !love(x,y) | !love(y,x) | happy(x)
axiom[3]>>> love(A,B)
axiom[4]>>> love(B,A)
axiom[5]>>> .
(human)>>> :set-history
(human)>>> happy(A)

Evaluation took:
  0.050 seconds of real time
  0.049196 seconds of total run time (0.049196 user, 0.000000 system)
  98.00% CPU
  303 lambdas converted
  98,007,272 processor cycles
  16,910,960 bytes consed

PROVABLE under the human

 □ ←← love(A,B)
 ↑
 ↑
 !love(A,B)  ←← love(B,A)
      ↑
      ↑
 !love(A,x) | !love(x,A)  ←← !happy(A)
             ↑
             ↑
 !love(y,x) | !love(x,y) | happy(y)
```

### 完備化された項書き換え系の元での等式証明例

入力された式が等式である場合は、完備化を試みます。
完備化が成功した場合は、項書き換え系の元で等式を証明することができます。

```
(NIL)>>> :def-axiom group
input . to finish definition
axiom[1]>>> plus(ZERO,x) = x
axiom[2]>>> plus(plus(x,y),z) = plus(x, plus(y,z))
axiom[3]>>> plus(i(x),x) = ZERO
axiom[4]>>> .
Detected that a set of equations has been inputted.
Do you want to execute completion algorithm?  (yes or no) yes

The completion process was successful:
i(plus(y,x))=>plus(i(x),i(y))
i(i(x))=>x
i(ZERO)=>ZERO
plus(x,ZERO)=>x
plus(x,i(x))=>ZERO
plus(x,plus(i(x),y))=>y
plus(i(x),plus(x,y))=>y
plus(ZERO,x)=>x
plus(plus(x,y),z)=>plus(x,plus(y,z))
plus(i(x),x)=>ZERO
(group)>>> plus(plus(x,y),plus(z,w)) = plus(x, plus(y, plus(z, w)))
The equation can be PROVED under the axiom group

irreducible form under the group:
plus(x,plus(y,plus(z,w))) = plus(x,plus(y,plus(z,w)))
```

## インストール

### バイナリ

LinuxとWindows向けにビルドしたバイナリをダウンロード可能です。

[Releases](https://github.com/moratori/clover/releases)

### Quicklispから

Common Lispの実行環境がある場合は、quicklispにてソースをロードして実行することも可能です。
なお、quicklispのパブリックなリポジトリには存在しないため、手動でlocal-projects配下にcloneしてロードする必要があります。

```
$ cd /path/to/quicklisp/local-projects/
$ git clone https://github.com/moratori/clover.git

> #+sbcl(ql:quickload :sb-cover)
To load "sb-cover":
  Load 1 ASDF system:
    sb-cover
; Loading "sb-cover"

(:SB-COVER)

> (ql:quickload :clover)
To load "clover":
  Load 1 ASDF system:
    clover
; Loading "clover"
...
(:CLOVER)

> (clover.repl:main)
Command help
    :help                    show this help
    :quit                    quit from REPL
    :def-axiom    <name>     define an axiomatic system <name>
    :show-axiom              enumerate all axiomatic system that are currently defined
    :set-axiom    <name>     set current axiomatic system to <name>
    :save-axiom   <path>     save curent axiomatic system to <path>
    :load-axiom   <path>     restore axiomatic systems from <path>
    :set-history             keep resolution history. this option automatically
                             enabled if save-tree option on.
    :unset-history           disable history
    :set-profiler            enable statistical profiler
    :unset-profiler          disable statistical profiler
    :save-tree    <path>     save Graphviz code to <path>
    :unsave-tree
(NIL)>>> 
```

## 式の入力形式

前提となる式を定義する際には、`<premise expression>`の形式で入力します。
それ以外の場合は、`<consequence expression>`の形式で入力します。


```
<premise expression> ::= <equation> 
                       | <premise logical expression>

<consequence expression> ::= <equation>
                           | <consequence logical expression>
```

```
<equation> ::= <term> "=" <term> 
             | <term> "!=" <term>

<premise logical expression> ::= <symbol> <argument>
                               | "!" <symbol> <argument>
                               | <premise logical expression> "|" <premise logical expression>

<consequence logical expression> ::= <symbol> <argument>
                                   | "!" <symbol> <argument>
                                   | <consequence logical expression> "&" <consequence logical expression>

<argument> ::= "(" ")"
             | "(" <term sequence> ")"

<term> ::= "[" "]"
         | "[" <term sequence> "]"
         | <constant>
         | <symbol>
         | <symbol> "(" <term sequence> ")"

<term sequence> ::= <term>
                  | <term sequence> "," <term>

<symbol>   ::= "a" | "b" | "c" | "d" | ...
<constant> ::= "A" | "B" | "C" | "D" | ...

```


## Author

* moratori

## Copyright

Copyright (c) 2018 moratori

