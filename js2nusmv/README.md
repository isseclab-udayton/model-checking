JS2NuSMV
========

JS2NuSMV converts a simple JavaScript program into a NuSMV module. Because of the
restrictions of the NuSMV input language, input scripts must follow a specific
format and only use certain permitted JavaScript constructs. For an example of a
valid input program, see `example.js`.

The only recognized values are integers (values for which `Number.isInteger(..)`
is true) and boolean values.
All variables must be declared and initialized at the start of the script.
Arithmetic and logical operators are banned due to their tricky semantics, so
custom functions defined in `prelude.js` must be used instead. Other recognized
JavaScript syntax elements are listed here:

* `=` variable assignment operator
* `if`-`else if`-`else` blocks
* `while` loops

Build & Run Instructions
------------------------

The requirements to build JS2NuSMV are:

* `make` build system
* Glasgow Haskell Compiler (`ghc`)
* `language-javascript` from Hackage

To build, just run `make` at the project root directory.
