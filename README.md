# Clover

[![Build Status](https://travis-ci.org/moratori/clover.svg?branch=master)](https://travis-ci.org/moratori/clover)

一階述語論理式(閉論理式)の充足不可能性を判定するツールです。  
判定には導出原理(Resolution Principle)を使用しています。  
充足不可能性を判定できた場合は、導出反駁木(Refutation Tree)を示すGraphvizのコードを出力することが可能です  

![sample proof figure](sample-refutation-tree.png)

## Usage

```
$ ./roswell/clover.ros
Command help
    :help                    show this help
    :quit                    quit from REPL
    :def-axiom    <name>     define an axiomatic system <name>
    :show-axiom              enumerate all axiomatic system that are currently defined
    :show-strategy           show the current strategy for resolution
    :set-axiom    <name>     set current axiomatic system to <name>
    :set-strategy <algorithm> <depth> set specific resolution algorithm
    :set-history             keep resolution history. this option automatically
                             enabled if save-tree option on.
    :unset-history           disable history
    :set-profiler            enable statistical profiler
    :unset-profiler          disable statistical profiler
    :save-tree    <path>     save Graphviz code tree to <path>
    :unsave-tree
(NIL)>>> :def-axiom human
input . to finish definition
axiom[1]>>> !human(x) | mortal(x)
axiom[2]>>> human(SOCRATES)
axiom[3]>>> .
(human)>>> :set-history
(human)>>> mortal(SOCRATES)

Evaluation took:
  0.060 seconds of real time
  0.058466 seconds of total run time (0.058466 user, 0.000000 system)
  96.67% CPU
  270 lambdas converted
  116,444,867 processor cycles
  15,177,920 bytes consed

PROVABLE under the human

 □ ←← human(SOCRATES)
 ↑
 ↑
 !human(SOCRATES)  ←← !mortal(SOCRATES)
         ↑
         ↑
 !human(v722) | mortal(v722) 
```

## Installation

### Roswellを使用する場合

```
$ cd ~/.roswell/local-projects/
$ git clone https://github.com/moratori/clover.git
$ ./clover/run-all-tests.sh
```

## Author

* moratori

## Copyright

Copyright (c) 2018 moratori

