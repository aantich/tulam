// AWFY Permute — Recursive Permutation Generation
// Generates all permutations of n elements, counting via a mutable counter.

function swap(v:MutArray(Int), i:Int, j:Int) : Unit =
    let { a = mutRead(v, i); b = mutRead(v, j); u = mutWrite(v, i, b) } in
    mutWrite(v, j, a);

function permuteInner(v:MutArray(Int), n:Int, count:Ref(Int)) : Unit =
    if n == 0 then modifyRef(count, fn(c) = c + 1)
    else permuteLoop(v, n, n, count);

function permuteLoop(v:MutArray(Int), n:Int, i:Int, count:Ref(Int)) : Unit =
    if i < 0 then Unit
    else
        let { u1 = swap(v, n, i); u2 = permuteInner(v, n - 1, count); u3 = swap(v, n, i) } in
        permuteLoop(v, n, i - 1, count);

function benchPermute() : Int =
    let { n = 6; v = newMutArray(n, 0); count = newRef(0); u = initPermuteArr(v, 0, n); u2 = permuteInner(v, n - 1, count) } in
    readRef(count);

function initPermuteArr(v:MutArray(Int), i:Int, n:Int) : Unit =
    if i >= n then Unit
    else
        let u = mutWrite(v, i, i) in
        initPermuteArr(v, i + 1, n);

function verifyPermute() : Bool = benchPermute() == 720;

function main() : Unit =
    let { result = benchPermute(); u = putStrLn("AWFY Permute: " ++ show(result)) } in
    if verifyPermute() then putStrLn("PASS")
    else putStrLn("FAIL: expected 720");
