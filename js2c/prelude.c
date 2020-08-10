/* JS2C prelude
 *
 * NOTE: This prelude is not complete. It only contains a sample
 * of definitions for demonstration purposes.
 */

enum JSType {JSnumber, JSboolean, JSundefined};

typedef union {
    double asNum;  // JSnumber
    int asBool;    // JSboolean
} JSVal;

typedef struct {
    enum JSType type;
    JSVal val;
} JSVar;

#define NAN (0.0 / 0.0)

#define jsTrue  ((JSVar){JSboolean, {.asBool = 1}})
#define jsFalse ((JSVar){JSboolean, {.asBool = 0}})
#define jsNaN ((JSVar){JSnumber, {.asNum = 0.0/0.0 }})
#define jsInfinity ((JSVar){JSnumber, {.asNum = 1.0 / 0.0}})
#define jsUndefined ((JSVar){JSundefined, {.asBool = 0}})
#define jsNumValue(x) ((JSVar){JSnumber, {.asNum = x}})

JSVar ToBoolean(JSVar v) {
    return ((v.type == JSboolean) ? v :
            (v.type == JSundefined) ? jsFalse :
            (v.val.asNum == 0) ? jsFalse :
            (v.val.asNum == NAN) ? jsFalse :
            jsTrue);
}

JSVar jsAnd(JSVar x, JSVar y) {
    return (ToBoolean(x).val.asBool ? y : x);
}

// end of prelude

