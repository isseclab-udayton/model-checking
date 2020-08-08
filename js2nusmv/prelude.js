/* prelude.js
 *
 * This file contains custom arithmetic and logical operator
 * definitions. The js2nusmv program will not recognize the builtin
 * JavaScript operators, so these must be used instead.
 *
 * NOTE: This is not complete, it only contains a few examples of prelude
 *       functions for reference.
 */

// 32-bit signed integer addition
function opADD(x, y) {
    if (!Number.isInteger(x) || !Number.isInteger(y))
        throw "Arguments must be integers";
    var result = x + y;
    if (result >= 2**31)
        result -= 2**32;
    else if (result < -(2**31))
        result += 2**32;
    return result;
}

// integer equality
function opEQU(x, y) {
    if (!Number.isInteger(x) || !Number.isInteger(y))
        throw "Arguments must be integers";
    return x == y;
}

// integer less-or-equal comparison
function opLE(x, y) {
    if (!Number.isInteger(x) || !Number.isInteger(y))
        throw "Arguments must be integers";
    return x <= y;
}
