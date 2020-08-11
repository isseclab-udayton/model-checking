/* JS2C prelude
 *
 * The prelude now includes most operators that are relevent to
 * Number, Boolean, and Undefined types.
 */

enum JSType {JSnumber, JSboolean, JSundefined};

typedef union {
    double asNum;   // JSnumber
    long asBool;    // JSboolean
    unsigned long asHex;
} JSVal;

typedef struct {
    enum JSType type;
    JSVal val;
} JSVar;

#define ISNAN(x) ((x >> 48) & 0x7FF8 == 0x7FF8)

#define jsTrue  ((JSVar){JSboolean, {.asBool = 1}})
#define jsFalse ((JSVar){JSboolean, {.asBool = 0}})
#define jsNaN ((JSVar){JSnumber, {.asNum = 0.0/0.0 }})
#define jsInfinity ((JSVar){JSnumber, {.asNum = 1.0 / 0.0}})
#define jsUndefined ((JSVar){JSundefined, {.asBool = 0}})
#define jsNumValue(x) ((JSVar){JSnumber, {.asNum = x}})

// Abstract operations
JSVar ToBoolean(JSVar v) {
    return ((v.type == JSboolean) ? v :
            (v.type == JSundefined) ? jsFalse :
            (v.val.asNum == 0.0) ? jsFalse :
            (ISNAN(v.val.asHex)) ? jsFalse :
            jsTrue);
}

JSVar ToNumber(JSVar v) {
    return ((v.type == JSnumber) ? v :
            (v.type == JSundefined) ? jsNaN :
            (v.val.asBool) ? jsNumValue(1.0) :
            jsNumValue(0.0));
}

JSVar ToNumeric(JSVar v) {
    return ToNumber(v);
}

JSVar StrictEqualityComparison(JSVar x, JSVar y) {
    if (x.type != y.type) return jsFalse;
    if (x.type == JSnumber)
        return (x.val.asNum == y.val.asNum) ? jsTrue : jsFalse;
    if (x.type == JSundefined) return jsTrue;
    if (x.type == JSboolean) {
        return ((!x.val.asBool && !y.val.asBool)
                || (x.val.asBool && y.val.asBool)) ? jsTrue :
                jsFalse;
    }
}

JSVar AbstractEqualityComparison(JSVar x, JSVar y) {
    if (x.type == y.type) return StrictEqualityComparison(x, y);
    if (x.type == JSboolean)
        return (ToNumber(x).val.asNum == y.val.asNum) ? jsTrue : jsFalse;
    if (y.type == JSboolean)
        return (ToNumber(y).val.asNum == x.val.asNum) ? jsTrue : jsFalse;
    return jsFalse;
}

JSVar AbstractRelationalComparison(JSVar x, JSVar y) {
    return (ToNumeric(x).val.asNum < ToNumeric(y).val.asNum) ? jsTrue : jsFalse;
}

// Arithmetic operators
JSVar jsADD(JSVar x, JSVar y) {
    return jsNumValue(ToNumeric(x).val.asNum + ToNumeric(y).val.asNum);
}

JSVar jsSUB(JSVar x, JSVar y) {
    return jsNumValue(ToNumeric(x).val.asNum - ToNumeric(y).val.asNum);
}

JSVar jsMUL(JSVar x, JSVar y) {
    return jsNumValue(ToNumeric(x).val.asNum * ToNumeric(y).val.asNum);
}

JSVar jsDIV(JSVar x, JSVar y) {
    return jsNumValue(ToNumeric(x).val.asNum / ToNumeric(y).val.asNum);
}

JSVar jsMOD(JSVar x, JSVar y) {
    double xn = ToNumeric(x).val.asNum;
    double yn = ToNumeric(y).val.asNum;
    return jsNumValue(xn - (long)(xn/yn)*yn);
}

JSVar jsPOS(JSVar x) {
    return ToNumber(x);
}

JSVar jsNEG(JSVar x) {
    return jsNumValue(-ToNumeric(x).val.asNum);
}

// Logical operators
JSVar jsAND(JSVar x, JSVar y) {
    return (ToBoolean(x).val.asBool) ? y : x;
}

JSVar jsOR(JSVar x, JSVar y) {
    return (ToBoolean(x).val.asBool) ? x : y;
}

// language-javascript parser does not support the ?? operator
//JSVar jsCOAL(JSVar x, JSVar y) {
//    return (x.type == JSundefined) ? y : x;
//}

JSVar jsNOT(JSVar x) {
    return (ToBoolean(x).val.asBool) ? jsFalse : jsTrue;
}

// Equality operators
JSVar jsSEQU(JSVar x, JSVar y) {
    return StrictEqualityComparison(x, y);
}

JSVar jsSNEQ(JSVar x, JSVar y) {
    return (StrictEqualityComparison(x, y).val.asBool) ? jsFalse : jsTrue;
}

JSVar jsEQU(JSVar x, JSVar y) {
    return AbstractEqualityComparison(x, y);
}

JSVar jsNEQ(JSVar x, JSVar y) {
    return (AbstractEqualityComparison(x, y).val.asBool) ? jsFalse : jsTrue;
}

// Relational operators
JSVar jsLT(JSVar x, JSVar y) {
    return AbstractRelationalComparison(x, y);
}

JSVar jsGT(JSVar x, JSVar y) {
    return AbstractRelationalComparison(y, x);
}

JSVar jsLE(JSVar x, JSVar y) {
    return (AbstractRelationalComparison(y, x).val.asBool) ? jsFalse : jsTrue;
}

JSVar jsGE(JSVar x, JSVar y) {
    return (AbstractRelationalComparison(x, y).val.asBool) ? jsFalse : jsTrue;
}

// Conditional operator (? :)
JSVar jsTER(JSVar c, JSVar x, JSVar y) {
    return (ToBoolean(c).val.asBool) ? x : y;
}

// end of prelude

