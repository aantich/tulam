// AWFY Sieve — Sieve of Eratosthenes
// Finds prime numbers up to 5000 using a mutable boolean array.

function markMultiples(flags:MutArray(Int), i:Int, k:Int, sz:Int) : Unit =
    if k >= sz then Unit
    else
        let u = mutWrite(flags, k, 0) in
        markMultiples(flags, i, k + i, sz);

function sieveLoop(flags:MutArray(Int), i:Int, sz:Int, count:Ref(Int)) : Unit =
    if i >= sz then Unit
    else
        let { v = mutRead(flags, i); u = if v == 1 then
            let { u2 = markMultiples(flags, i, i + i, sz); u3 = modifyRef(count, fn(c) = c + 1) } in
            Unit
        else Unit } in
        sieveLoop(flags, i + 1, sz, count);

function sieve(sz:Int) : Int =
    let { flags = newMutArray(sz, 1); u = mutWrite(flags, 0, 0); u2 = mutWrite(flags, 1, 0); count = newRef(0); u3 = sieveLoop(flags, 2, sz, count) } in
    readRef(count);

function benchSieve() : Int = sieve(5000);

function verifySieve() : Bool = benchSieve() == 669;

function main() : Unit =
    let { result = benchSieve(); u = putStrLn("AWFY Sieve: " ++ show(result)) } in
    if verifySieve() then putStrLn("PASS")
    else putStrLn("FAIL: expected 669");
