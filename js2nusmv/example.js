var a = 1;
var b = 1;
var temp = 0;

while (a < 100) {
    temp = a + b;
    b = a;
    a = temp;
}
