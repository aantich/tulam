// P34: Pi Types — dependent function type support

// --- Form 1: Function with dependent return type ---
function depReturn(n:Nat, x:Nat) : Nat = plus(n, x);

// --- Form 2: Non-dependent arrow types still work ---
function simpleArrow(x:Int) : Int = x + 1;

// --- Form 3: Higher-order with arrow type parameter ---
function applyFn(f: Int -> Int, x:Int) : Int = f(x);

// --- Form 4: Dependent Pi type in a function parameter ---
function applyDep(f: (n:Nat) -> Nat, k:Nat) : Nat = f(k);

// --- Test functions ---
function t1() : Nat = depReturn(Succ(Z), Succ(Succ(Z)));
function t2() : Int = simpleArrow(41);
function t3() : Int = applyFn(simpleArrow, 9);

// t4: applying a dependent function through higher-order
function depMaker(n:Nat) : Nat = plus(n, Succ(Z));
function t4() : Nat = applyDep(depMaker, Succ(Z));

function nested(x:Int, y:Int) : Int = x + y;
function t5() : Int = nested(3, 4);
function applyToTen(f: (Int -> Int)) : Int = f(10);
function t6() : Int = applyToTen(simpleArrow);

// t7: dep Pi in parameter, different function
function t7() : Nat = applyDep(depMaker, Succ(Succ(Z)));

function depChain(n:Nat, m:Nat) : Nat = plus(n, m);
function t8() : Nat = depChain(Succ(Succ(Z)), Succ(Z));
