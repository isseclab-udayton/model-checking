JS2NuSMV
========

JS2NuSMV converts simple JavaScript programs into a NuSMV modules. To accomplish this, programs are not translated directly, but instead modified by replacing builtin JavaScript operators with calls to custom functions that more closely match the semantics of NuSMV expressions. NuSMV can then be used to verify that the modified script is correct. Because of the restrictions of the NuSMV input language, input scripts must follow a specific format and only use certain permitted JavaScript constructs. For an example of a valid input program, see `example.js`.

The only recognized values are integers (values for which `Number.isInteger(..)` is true) and boolean values. All variables must be declared and initialized at the start of the script. Custom functions defined in `prelude.js` are recognized along with their corresponding JavaScript operators. Other recognized JavaScript syntax elements are listed here:

* `+ - * / %` arithmetic operators
* `&& || ! == !=` logical operators
* `== != >= <= > <` integer comparison operators
* `= += -= *= /= %=` variable assignment operators
* `if`-`else if`-`else` blocks
* `while` loops

Build & Run Instructions
------------------------

The requirements to build JS2NuSMV are:

* `make` build system
* Glasgow Haskell Compiler (`ghc`)
* `language-javascript` from Hackage

To build, just run `make` at the project root directory.
Run `js2nusmv -help` for usage.
