// AWFY Storage — Recursive Tree Building
// Builds a random tree and counts nodes using a mutable counter.
// Uses the LCG random number generator from AWFY_Random.

type StorageTree = SLeaf * sz:Int + SBranch * children:List(StorageTree);

function buildTreeDepth(depth:Int, seed:Ref(Int), count:Ref(Int)) : StorageTree =
    let u = modifyRef(count, fn(c) = c + 1) in
    if depth == 0 then
        let r = randomNext(seed) in
        SLeaf(r)
    else
        let kids = buildChildren(4, depth - 1, seed, count, Nil) in
        SBranch(kids);

function buildChildren(n:Int, depth:Int, seed:Ref(Int), count:Ref(Int), acc:List(StorageTree)) : List(StorageTree) =
    if n <= 0 then acc
    else
        let child = buildTreeDepth(depth, seed, count) in
        buildChildren(n - 1, depth, seed, count, Cons(child, acc));

function benchStorage() : Int =
    let seed = newRef(74755) in
    let count = newRef(0) in
    let tree = buildTreeDepth(7, seed, count) in
    readRef(count);

function verifyStorage() : Bool = benchStorage() == 5461;
