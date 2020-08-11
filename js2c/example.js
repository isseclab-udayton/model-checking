var x, y;
var error = false;

function f(a, b) {
    var newA = a || 1;
    var newB = b || 1;
    return a * b;
}

x = -10;
while (x <= 10) {
    y = -10;
    while (y <= 10) {
        if (!f(x, y)) {
            ERROR: error = true;
        }
        y += 1;
    }
    x += 1;
}
