# Clover

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

> [!WARNING]
> **Function symbols in `.trs` input are currently case-insensitive.**
> The parser converts every symbol name to upper case before using it, so
> function symbols that differ only in letter case (for example `T` and `t`) are
> treated as the same function symbol. Inputs that rely on case to distinguish
> function symbols are therefore mis-parsed, and completion may report a spurious
> "success" for a collapsed system that is not the one given. See
> [issue #21](https://github.com/moratori/clover/issues/21) for details.
>
> This limitation applies to `.trs` batch input only. In the REPL
> (premise/consequence expressions) letter case is significant, but with a
> different, intentional meaning: names written in upper case are read as
> constants, while names written in lower case are read as variables or function
> symbols (see [Input format](#input-format) below).

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
<premise expression> ::= <formula>
                       | <premise expression> "|" <formula>

<consequence expression> ::= <formula>
                           | <consequence expression> "&" <formula>

<formula> ::= <equation>
            | <predicate>

<equation> ::= <term> "=" <term>
             | <term> "!=" <term>

<predicate> ::= <symbol> <argument>
              | "!" <symbol> <argument>

<argument> ::= "(" ")"
             | "(" <term sequence> ")"

<term> ::= "[" "]"
         | "[" <term sequence> "]"
         | <constant>
         | <symbol>
         | <symbol> "(" <term sequence> ")"

<term sequence> ::= <term>
                  | <term sequence> "," <term>

<symbol>   ::= a non-empty sequence of  a-z 0-9 _ + * . - / @ \
<constant> ::= a non-empty sequence of  A-Z
```

Depending on where it appears, a `<symbol>` denotes a variable, a function
symbol, or a predicate; a `<constant>` (an upper-case name) is always a constant.
Premises and consequences share the same `<formula>`s and differ only in the
connective that joins them (`|` for premises, `&` for consequences).

### File format for command-line batch execution

```
<trs> ::= <section>
        | <trs> <section>

<section> ::= "(VAR" <var name>* ")"
            | "(RULES" <rule>+ ")"
            | "(COMMENT" <text> ")"
            | "(FROM" <text> ")"

<rule> ::= <term> "->" <term>

<term> ::= <symbol>
         | <symbol> "(" ")"
         | <symbol> "(" <term sequence> ")"

<term sequence> ::= <term>
                  | <term sequence> "," <term>

<var name> ::= a non-empty sequence of  a-z A-Z 0-9 _
<symbol>   ::= a non-empty sequence of  a-z A-Z 0-9 _ + * . - / @ & \
```

Exactly one `(VAR ...)` section and exactly one `(RULES ...)` section are
required; `(COMMENT ...)` and `(FROM ...)` sections are optional and may appear
any number of times, in any order. A bare `<symbol>` is a variable when it is
listed in the `(VAR ...)` section and a constant otherwise; a parenthesised
`<symbol> "(" ... ")"` is a function application (a constant when the argument
list is empty). Unlike the REPL form, variables are declared explicitly in
`(VAR ...)` rather than inferred from letter case —
**[but see the case-sensitivity warning above](#running-batch-completion-from-the-command-line)**.

## Author

* moratori

## Copyright

Copyright (c) 2018 moratori
