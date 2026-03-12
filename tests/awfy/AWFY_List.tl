// AWFY List — Linked List Recursion Benchmark
// Purely functional benchmark using tulam's native List type.
// Builds 3 lists, repeatedly applies tail operation, checks result length.

function makeList(n:Int) : List(Int) =
    if n == 0 then Nil
    else Cons(n, makeList(n - 1));

function listLength(xs:List(Int)) : Int = match xs
    | Nil -> 0
    | Cons(h, t) -> 1 + listLength(t);

function listIsNil(xs:List(Int)) : Bool = match xs
    | Nil -> True
    | Cons(h, t) -> False;

function listTail(xs:List(Int)) : List(Int) = match xs
    | Nil -> Nil
    | Cons(h, t) -> t;

function isShorterThan(x:List(Int), y:List(Int)) : Bool =
    if listIsNil(y) then False
    else if listIsNil(x) then True
    else isShorterThan(listTail(x), listTail(y));

function tailFn(x:List(Int), y:List(Int), z:List(Int)) : List(Int) =
    if isShorterThan(y, x) then
        tailFn(tailFn(listTail(x), y, z), tailFn(listTail(y), z, x), tailFn(listTail(z), x, y))
    else z;

function benchList() : Int =
    let result = tailFn(makeList(15), makeList(10), makeList(6)) in
    listLength(result);

function verifyList() : Bool = benchList() == 10;

function main() : Unit =
    let { result = benchList(); u = putStrLn("AWFY List: " ++ show(result)) } in
    if verifyList() then putStrLn("PASS")
    else putStrLn("FAIL: expected 10");
