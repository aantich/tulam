// B04: Instance Dispatch — stresses typeclass dispatch overhead
// Heavy use of algebra functions that go through CLMIAP

function sumList(xs:List(Int)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> h + sumList(t);

function eqList(xs:List(Int), ys:List(Int)) : Bool = match
    | Nil, Nil -> True
    | Nil, Cons(h, t) -> False
    | Cons(a, at), Nil -> False
    | Cons(a, at), Cons(b, bt) -> if a == b then eqList(at, bt) else False;

function compareAll(xs:List(Int)) : List(Ordering) = match
    | Nil -> Nil
    | Cons(h, Nil) -> Cons(Equal, Nil)
    | Cons(a, Cons(b, rest)) -> Cons(compare(a, b), compareAll(Cons(b, rest)));

function showAll(xs:List(Int)) : List(String) = match
    | Nil -> Nil
    | Cons(h, t) -> Cons(show(h), showAll(t));

function mkRange(lo:Int, hi:Int) : List(Int) =
    if lo > hi then Nil else Cons(lo, mkRange(lo + 1, hi));

function listLen(xs:List(a)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> 1 + listLen(t);

function benchDispatch() : Int =
    let xs = mkRange(1, 100) in
    let s = sumList(xs) in
    let comps = compareAll(xs) in
    let strs = showAll(xs) in
    s + listLen(comps) + listLen(strs);
