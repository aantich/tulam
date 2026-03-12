// AWFY Queens — N-Queens Problem (8x8)
// Backtracking algorithm using mutable arrays for constraint tracking.

function initArr(a:MutArray(Int), i:Int, n:Int, val:Int) : Unit =
    if i >= n then Unit
    else
        let u = mutWrite(a, i, val) in
        initArr(a, i + 1, n, val);

function getRowCol(r:Int, c:Int, n:Int) : Bool =
    True;

function placeQueen(c:Int, n:Int, queenRows:MutArray(Int), freeRows:MutArray(Int), freeMaxs:MutArray(Int), freeMins:MutArray(Int)) : Bool =
    if c == n then True
    else tryRow(0, c, n, queenRows, freeRows, freeMaxs, freeMins);

function tryRow(r:Int, c:Int, n:Int, queenRows:MutArray(Int), freeRows:MutArray(Int), freeMaxs:MutArray(Int), freeMins:MutArray(Int)) : Bool =
    if r == n then False
    else
        let { fr = mutRead(freeRows, r); fmx = mutRead(freeMaxs, r + c); fmn = mutRead(freeMins, r - c + n - 1) } in
        if fr == 1 then
            if fmx == 1 then
                if fmn == 1 then
                    let {
                        u1 = mutWrite(queenRows, c, r);
                        u2 = mutWrite(freeRows, r, 0);
                        u3 = mutWrite(freeMaxs, r + c, 0);
                        u4 = mutWrite(freeMins, r - c + n - 1, 0);
                    } in
                    if placeQueen(c + 1, n, queenRows, freeRows, freeMaxs, freeMins) then True
                    else
                        let {
                            u5 = mutWrite(freeRows, r, 1);
                            u6 = mutWrite(freeMaxs, r + c, 1);
                            u7 = mutWrite(freeMins, r - c + n - 1, 1);
                        } in
                        tryRow(r + 1, c, n, queenRows, freeRows, freeMaxs, freeMins)
                else tryRow(r + 1, c, n, queenRows, freeRows, freeMaxs, freeMins)
            else tryRow(r + 1, c, n, queenRows, freeRows, freeMaxs, freeMins)
        else tryRow(r + 1, c, n, queenRows, freeRows, freeMaxs, freeMins);

function queens(n:Int) : Bool =
    let {
        queenRows = newMutArray(n, 0);
        freeRows = newMutArray(n, 1);
        freeMaxs = newMutArray(2 * n, 1);
        freeMins = newMutArray(2 * n, 1);
    } in
    placeQueen(0, n, queenRows, freeRows, freeMaxs, freeMins);

function queensRepeat(i:Int, n:Int) : Bool =
    if i <= 0 then True
    else
        if queens(n) then queensRepeat(i - 1, n)
        else False;

function benchQueens() : Bool = queensRepeat(10, 8);

function verifyQueens() : Bool = benchQueens() == True;

function main() : Unit =
    let { result = benchQueens(); u = putStrLn("AWFY Queens: " ++ show(result)) } in
    if verifyQueens() then putStrLn("PASS")
    else putStrLn("FAIL: expected True");
