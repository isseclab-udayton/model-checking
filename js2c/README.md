JS2C
====

JS2C converts simple Javascript code into C code while precisely preserving
the original Javascript semantics. The output C program can be analyzed
using a model checker such as CPAchecker, and the analysis results will
be valid for the original Javascript.

Build Requirements
------------------

* `make` build system
* Glasgow Haskell Compiler (`ghc`)
* `language-javascript` package from Hackage

Build Instructions
------------------

Just run `make` at the project root directory.

Usage
-----

Run `./js2c --help` for usage
