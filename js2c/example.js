var x = -10;
var y;
var error = false;

function f(a, b) {
    var newA = a || 1;
    var newB = b || 1;
    return a * b;
}

while (x <= 10) {
    y = -10;
    while (y <= 10) {
        if (!f(x)) {
            ERROR: error = true;
        }
        y += 1;
    }
    x += 1;
}
