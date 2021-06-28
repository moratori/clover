# Clover

[![Build Status](https://travis-ci.org/moratori/clover.svg?branch=master)](https://travis-ci.org/moratori/clover)

## 概要

一階述語論理式の充足不可能性を判定するツールです。  
判定には導出原理(Resolution Principle)を使用しています。  
判定できた場合は、導出反駁木(Refutation Tree)を示すGraphvizのコードを出力することが可能です  

![sample proof figure](sample-refutation-tree.png)

## 使い方

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

## インストール

### バイナリ

[Releases](https://github.com/moratori/clover/releases)

### ソースから

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

## Author

* moratori

## Copyright

Copyright (c) 2018 moratori

