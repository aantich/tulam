// B01: Arithmetic — stresses constant folding
// Many literal arithmetic operations that can be folded at compile time

function arithChain() : Int =
    let { a = 1 + 2 + 3 + 4 + 5; b = 10 * 20 * 30; c = 100 - 50 - 25 } in
    a + b + c;

// Recursive computation with arithmetic — tests beta reduction + eval
function sumTo(n:Int) : Int =
    if n <= 0 then 0 else n + sumTo(n - 1);

function benchArith() : Int = sumTo(200);
