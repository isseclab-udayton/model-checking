var a = 1;
var b = 1;
var temp = 0;

while (opLE(a, 100)) {
    temp = opADD(a, b);
    b = a;
    a = temp;
}
