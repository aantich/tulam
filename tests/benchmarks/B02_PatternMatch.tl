// B02: Pattern Matching — stresses known-constructor + dead-code-elim
// Deep pattern matching over algebraic types

function natToInt(n:Nat) : Int = match
    | Z -> 0
    | Succ(m) -> 1 + natToInt(m);

function intToNat(n:Int) : Nat =
    if n <= 0 then Z else Succ(intToNat(n - 1));

function addNat(a:Nat, b:Nat) : Nat = match
    | Z, b -> b
    | Succ(m), b -> Succ(addNat(m, b));

function mulNat(a:Nat, b:Nat) : Nat = match
    | Z, b -> Z
    | Succ(m), b -> addNat(b, mulNat(m, b));

function benchPattern() : Int =
    let { n8 = intToNat(8); n6 = intToNat(6) } in
    natToInt(mulNat(n8, n6));

// List operations with heavy pattern matching
function listLength(xs:List(a)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> 1 + listLength(t);

function listMap(f:a -> b, xs:List(a)) : List(b) = match
    | f, Nil -> Nil
    | f, Cons(h, t) -> Cons(f(h), listMap(f, t));

function listFilter(f:a -> Bool, xs:List(a)) : List(a) = match
    | f, Nil -> Nil
    | f, Cons(h, t) -> if f(h) then Cons(h, listFilter(f, t)) else listFilter(f, t);

function mkList(n:Int) : List(Int) =
    if n <= 0 then Nil else Cons(n, mkList(n - 1));

function benchListOps() : Int =
    let { xs = mkList(200); doubled = listMap(fn(x) = x * 2, xs); filtered = listFilter(fn(x) = x > 100, doubled) } in
    listLength(filtered);
