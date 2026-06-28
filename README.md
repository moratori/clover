# Clover

[![Build Status](https://travis-ci.org/moratori/clover.svg?branch=master)](https://travis-ci.org/moratori/clover)

## Overview

Clover is an experimental automated theorem prover for first-order predicate
logic, implemented in Common Lisp. It implements the following algorithms:

* The Knuth-Bendix completion algorithm
* The resolution principle

## Installation

### Binary

Prebuilt binaries for Linux and Windows are available for download.

[Releases](https://github.com/moratori/clover/releases)

### From source

Clover is **not** registered in the public Quicklisp distribution, so it cannot
be installed with `ql:quickload` out of the box. You need to place the source
where ASDF can find it and load it manually.

1. Put the `clover` repository in a location that ASDF searches, such as your
   local-projects directory (for Roswell this is typically
   `~/.roswell/local-projects/`, for a standard Quicklisp setup
   `~/quicklisp/local-projects/`):

   ```sh
   cd ~/.roswell/local-projects/   # or ~/quicklisp/local-projects/
   git clone https://github.com/moratori/clover.git
   ```

2. Clover depends on the `lexer` library, which is **also not available in
   Quicklisp**. Clone it into the same local-projects directory so that ASDF can
   resolve the dependency:

   ```sh
   cd ~/.roswell/local-projects/   # or ~/quicklisp/local-projects/
   git clone https://github.com/massung/lexer.git
   ```

   All other dependencies are available from Quicklisp and will be downloaded
   automatically.

3. Load Clover from the REPL:

   ```lisp
   (ql:quickload :clover)
   ```

   To load the test system instead, use `(ql:quickload :clover-test)`.

## Running the tests

Clover uses the [1am](https://github.com/lmj/1am) test framework. The test suite
lives under `tests/` and is defined by the `clover-test` ASDF system.

### Running the full suite

The simplest way is the helper script at the repository root:

```sh
./run-all-tests.sh
```

This requires Roswell and the `timeout` command. On SBCL it additionally
generates `sb-cover` coverage (written under `coverage/`) and an `sb-sprof`
profile; if Graphviz (`dot`) is installed, refutation trees are rendered as PNG
images. The whole run is bounded by a wall-clock timeout (`test_duration_time`
in the script).

### Running from the REPL

Load the test system and run every registered test:

```lisp
(ql:quickload :clover-test)
(1am:run)
```

Each test is an ordinary function, so a single test can be run by calling its
symbol directly:

```lisp
(clover.tests.unify::clover.tests.unify.subsumption-clause-p.boundary)
```

To run only the tests belonging to a particular package, filter `1am:*tests*`
by `symbol-package` and pass the result to `1am:run`:

```lisp
(1am:run
  (remove-if-not
    (lambda (test)
      (string= "CLOVER.TESTS.SIMPLIFY"
               (package-name (symbol-package test))))
    1am:*tests*))
```

### Completion benchmark corpus

The package `clover.tests.completion.corpus` contains regression tests built from
the equational-system benchmarks published on the JAIST Maximal Completion
experiments page:

* <https://www.jaist.ac.jp/project/maxcomp/experiments/experiments.html>

The corresponding `.trs` files (originating from TPDB and the literature) are
bundled under `tests/resources/eq_systems/`. Each test simply asserts that the
current build of Clover completes the system. Only the subset that completes
quickly and reliably is included; harder instances are intentionally omitted to
keep the suite fast and non-flaky.

## Running batch completion from the command line

The completion procedure can be executed in batch mode from the command line.

```
$ cat input_file.txt
(VAR x y z)
(RULES
c -> a
g(x) -> x
f(x, b) -> x
f(x, g(y)) -> f(g(x), y)
f(b, z) -> c
)
$ ./clover-linux-x86_64_ver2.4.0 input_file.txt
YES

(VAR x)
(RULES
 f(x,A) -> x
 f(A,x) -> A
 B -> A
 g(x) -> x
 C -> A
)
(COMMENT
 A < B < C < F < G
)
$
```

## Running the REPL

You can run proofs using either the resolution principle or completion.

Use the `:def-axiom` command to define the premises of the formula you want to
prove. After that, entering the formula to be proved starts the proof attempt.

### Example: proof by the resolution principle

```
$ ./clover-linux-x86_64_ver2.2.1
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

### Example: equational proof

When the entered formula is an equation, Clover attempts completion.
If completion succeeds, the equation can be proved under the resulting term
rewriting system.

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

## Input format

### REPL

When defining a premise, the input follows the `<premise expression>` form.
Otherwise, the input follows the `<consequence expression>` form.

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

<symbol>   ::= "a" | "b" | "c" | "d" | ... | "z" | ... | "aaa" | ...
<constant> ::= "A" | "B" | "C" | "D" | ... | "Z" | ... | "AAA" | ...

```

### File format for command-line batch execution

```
todo
```

## Author

* moratori

## Copyright

Copyright (c) 2018 moratori
