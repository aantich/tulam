module AWFY_Storage;
import AWFY_Random;
import Harness;

// AWFY Storage — Recursive Tree Building
// Builds a random tree and counts nodes using a mutable counter.

type StorageTree = SLeaf * sz:Int + SBranch * children:List(StorageTree);

function buildTreeDepth(depth:Int, seed:Ref(Int), count:Ref(Int)) : StorageTree =
    if depth == 0 then
        let r = randomNext(seed) in
        SLeaf(r)
    else
        let { u = modifyRef(count, fn(c) = c + 1); kids = buildChildren(4, depth - 1, seed, count, Nil) } in
        SBranch(kids);

function buildChildren(n:Int, depth:Int, seed:Ref(Int), count:Ref(Int), acc:List(StorageTree)) : List(StorageTree) =
    if n <= 0 then acc
    else
        let child = buildTreeDepth(depth, seed, count) in
        buildChildren(n - 1, depth, seed, count, Cons(child, acc));

function benchStorage() : Int =
    let { seed = newRef(74755); count = newRef(0); tree = buildTreeDepth(7, seed, count) } in
    readRef(count);

function verifyStorage() : Bool = benchStorage() == 5461;

function benchIter(name:String, n:Int, i:Int, total:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let {
            start = clockNanos();
            result = benchStorage();
            end = clockNanos();
            elapsed = div(end - start, 1000);
            ok = result == 5461;
            u1 = if ok then Unit else putStrLn("ERROR: verification failed");
            u2 = benchOnce(name, result, elapsed, i);
            u3 = modifyRef(total, fn(t) = t + elapsed)
        } in benchIter(name, n, i + 1, total);

function main() : Unit =
    let {
        u1 = putStrLn("Starting Storage benchmark ...");
        total = newRef(0);
        u2 = benchIter("Storage", 10, 0, total);
        t = readRef(total)
    } in benchSummary("Storage", t, 10);
