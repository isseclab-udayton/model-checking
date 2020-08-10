# JS2C

JS2C converts simple JavaScript code into C code while precisely preserving the original JavaScript semantics. The output C program can be analyzed using a model checker such as CPAchecker, and the analysis results will be valid for the original JavaScript.

## Supported Features

JavaScript input programs must follow certain guidelines to have a valid C translation. All global functions and variables must be declared at the beginning of the script. The first statement that is not a function or variable declaration will be taken as the start of the `main` function. Further variable declarations will be local to `main` and further function declarations are prohibited. See `example.js` for a sample input program. Here is the list of supported JavaScript statements and expressions:

* values of type `Number`, `Boolean`, `Undefined`
* `+ - * / %` arithmetic operators
* unary `-` operator
* `== != >= <= > <` comparison operators
* `&& || !` logical operators
* `= += -= *= /= %=` assignment operators
* `if`-`else if`-`else` blocks
* `while` blocks
* statement labels
* `return` statements

## Build & Run 

Build requirements are

* `make` build system
* Glasgow Haskell Compiler (`ghc`)
* `language-javascript` package from Hackage

To build, just run `make` at the project root directory.

Run `./js2c --help` for usage.

