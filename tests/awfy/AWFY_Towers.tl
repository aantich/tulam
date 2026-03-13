module AWFY_Towers;
import Harness;

// AWFY Towers — Tower of Hanoi
// Recursive disk moves between 3 pegs using linked-list stacks.

type Disk = DiskNone + DiskVal * dsize:Int * next:Disk;

function pushDisk(piles:MutArray(Disk), peg:Int, diskSize:Int) : Unit =
    let top = mutRead(piles, peg) in
    mutWrite(piles, peg, DiskVal(diskSize, top));

function popDisk(piles:MutArray(Disk), peg:Int) : Int =
    let top = mutRead(piles, peg) in
    match top
        | DiskNone -> 0 - 1
        | DiskVal(sz, rest) ->
            let u = mutWrite(piles, peg, rest) in
            sz;

function moveTop(piles:MutArray(Disk), from:Int, to:Int, moves:Ref(Int)) : Unit =
    let { d = popDisk(piles, from); u = pushDisk(piles, to, d) } in
    modifyRef(moves, fn(m) = m + 1);

function moveDisks(n:Int, from:Int, to:Int, other:Int, piles:MutArray(Disk), moves:Ref(Int)) : Unit =
    if n == 1 then moveTop(piles, from, to, moves)
    else
        let { u1 = moveDisks(n - 1, from, other, to, piles, moves); u2 = moveTop(piles, from, to, moves) } in
        moveDisks(n - 1, other, to, from, piles, moves);

function initPiles(piles:MutArray(Disk), i:Int, numDisks:Int) : Unit =
    if i > numDisks then Unit
    else
        let u = pushDisk(piles, 0, numDisks - i + 1) in
        initPiles(piles, i + 1, numDisks);

function benchTowers() : Int =
    let {
        numDisks = 13;
        piles = newMutArray(3, DiskNone);
        u = initPiles(piles, 1, numDisks);
        moves = newRef(0);
        u2 = moveDisks(numDisks, 0, 2, 1, piles, moves);
    } in
    readRef(moves);

function verifyTowers() : Bool = benchTowers() == 8191;

function benchIter(name:String, n:Int, i:Int, total:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let {
            start = clockNanos();
            result = benchTowers();
            end = clockNanos();
            elapsed = div(end - start, 1000);
            ok = result == 8191;
            u1 = if ok then Unit else putStrLn("ERROR: verification failed");
            u2 = benchOnce(name, result, elapsed, i);
            u3 = modifyRef(total, fn(t) = t + elapsed)
        } in benchIter(name, n, i + 1, total);

function main() : Unit =
    let {
        u1 = putStrLn("Starting Towers benchmark ...");
        total = newRef(0);
        u2 = benchIter("Towers", 10, 0, total);
        t = readRef(total)
    } in benchSummary("Towers", t, 10);
