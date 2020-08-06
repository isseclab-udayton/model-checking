
# Model-checking tool chain for JavaScript

## Zach Rowland and Phu H. Phung

## Intelligent Systems Security Lab

## Department of Computer Science, University of Dayton.

This repository describes two possible methods for translating JavaScript into a language that can be model-checked. The first method is a more direct translation from JavaScript to C. The second method requires imposing more restrictions on the JavaScript source program which enables translation to more restrictive modeling languages like Promela or NuSMV.

## [JS2C](js2c): Translation from JavaScript to C. 

The resulting
C program can then be run through a C static analysis tool such as [CPAchecker](https://cpachecker.sosy-lab.org/),
and the results of the analysis should apply to the original Javascript. The C language is powerful enough to emulate key
Javascript features like dynamic typing and floating point arithmetic. Dynamic typing is emulated in C by intruducing
the `JSVar` structure which contains a Javascript type along with a value. Values of primitive types in Javascript
could easily be mapped to instances of `JSVar` in C. Likewise, all Javascript operators and functions could be mapped to C
functions that operate on `JSVar` instances. `JSVar` could also be extended to represent Javascript objects and strings
by using the heap. First-class functions could possibly be implemented using function pointers. More advanced features
like dynamic code generation and the `eval` construct might not be feasibly translated.

## [JS2NuSMV](js2nusmv): Translation from JavaScript to NuSMV

The second method encompasses any translation from JavaScript to a general-purpose model checker like SPIN or NuSMV with
a simple, dedicated input language. The basic idea is to alter the semantics of Javascript to be compatible with the
target modeling language without altering the Javascript syntax. This can be accomplished by defining custom Javascript
functions that are intended to replace the original builtin operators and common library functions. An automated system
could then walk through a Javascript program and replace the builtin operators and library functions with their corresponding
custom functions. For example, a function
`add32` could be defined in Javascript that performs 32-bit signed integer arithmetic and throws an exception for invalid
arguments. The automated system would then translate expressions like `3 + 2` into `add32(3, 2)`. The modified Javascript
program could then be translated into a modeling language. If the analysis indicates the program is safe, then the _modified_
Javascript program is safe and can be used, but the _original_ Javascript program would be disregarded. This method provides the
benefit of flexibility, but places heavy restrictions on what constructs are allowed in a Javascript input program. Features like
dynamic typing, floating-point arithmetic, and function calls are often difficult to implement in modeling languages, and
implementing dynamic code generation and `eval` seems practically impossible.


Both of these methods require at least _some_ restrictions to be placed on Javascript input programs. Currently, Javascript is
just too complex for model-checking the entire specification to be worth the effort. The goal is to construct a system for
analyzing a portion of the specification that is flexible enough to enforce most useful security policies.
