// AWFY Permute — Recursive Permutation Generation
// Generates all permutations of n elements, counting via a mutable counter.

function swap(v:MutArray(Int), i:Int, j:Int) : Unit =
    let a = mutRead(v, i) in
    let b = mutRead(v, j) in
    let u = mutWrite(v, i, b) in
    mutWrite(v, j, a);

function permuteInner(v:MutArray(Int), n:Int, count:Ref(Int)) : Unit =
    if n == 0 then modifyRef(count, fn(c) = c + 1)
    else permuteLoop(v, n, n, count);

function permuteLoop(v:MutArray(Int), n:Int, i:Int, count:Ref(Int)) : Unit =
    if i < 0 then Unit
    else
        let u1 = swap(v, n, i) in
        let u2 = permuteInner(v, n - 1, count) in
        let u3 = swap(v, n, i) in
        permuteLoop(v, n, i - 1, count);

function benchPermute() : Int =
    let n = 6 in
    let v = newMutArray(n, 0) in
    let count = newRef(0) in
    let u = initPermuteArr(v, 0, n) in
    let u2 = permuteInner(v, n - 1, count) in
    readRef(count);

function initPermuteArr(v:MutArray(Int), i:Int, n:Int) : Unit =
    if i >= n then Unit
    else
        let u = mutWrite(v, i, i) in
        initPermuteArr(v, i + 1, n);

function verifyPermute() : Bool = benchPermute() == 720;
