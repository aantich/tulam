// B03: Higher-Order Functions — stresses eta-reduce + inlining + beta reduction
// Many wrapper functions and function composition patterns

function twice(f:a -> a, x:a) : a = f(f(x));
function thrice(f:a -> a, x:a) : a = f(f(f(x)));
function incr(x:Int) : Int = x + 1;
function double(x:Int) : Int = x * 2;

function applyN(f:Int -> Int, n:Int, x:Int) : Int =
    if n <= 0 then x else applyN(f, n - 1, f(x));

function benchHOF() : Int =
    let a = applyN(incr, 200, 0) in
    let b = applyN(double, 10, 1) in
    let c = twice(fn(x) = x + 5, 0) in
    let d = thrice(fn(x) = x * 2, 1) in
    a + b + c + d;

// Composition chains
function comp(f:b -> c, g:a -> b, x:a) : c = f(g(x));

function benchComp() : Int =
    let f1 = fn(x) = x + 1 in
    let f2 = fn(x) = x * 2 in
    let f3 = fn(x) = x - 3 in
    comp(f3, comp(f2, f1, 10), 0);
