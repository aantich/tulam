module AWFY_Mandelbrot;
import Harness;

// AWFY Mandelbrot — Mandelbrot Set Computation
// Escape-time iteration over a grid with bit-packed checksum.

function toFloat(n:Int) : Float64 = __sitofp_i64(n);

function mandelbrotIter(cr:Float64, ci:Float64, zr:Float64, zi:Float64, i:Int, limit:Int) : Int =
    if i >= limit then 1
    else
        let { zr2 = zr * zr; zi2 = zi * zi } in
        if zr2 + zi2 > 4.0 then 0
        else
            let { nzr = zr2 - zi2 + cr; nzi = 2.0 * zr * zi + ci } in
            mandelbrotIter(cr, ci, nzr, nzi, i + 1, limit);

function mandelbrotRow(x:Int, y:Int, sz:Int, sum:Ref(Int), byteAcc:Ref(Int), bitNum:Ref(Int)) : Unit =
    if x >= sz then Unit
    else
        let {
            cr = 2.0 * toFloat(x) / toFloat(sz) - 1.5;
            ci = 2.0 * toFloat(y) / toFloat(sz) - 1.0;
            pixel = mandelbrotIter(cr, ci, 0.0, 0.0, 0, 50);
            ba = readRef(byteAcc);
            newBa = shiftL(ba, 1) .|. pixel;
            u1 = writeRef(byteAcc, newBa);
            bn = readRef(bitNum) + 1;
            u2 = writeRef(bitNum, bn);
        } in
        let u3 = if bn == 8 then
            let { u4 = modifyRef(sum, fn(s) = xor(s, newBa)); u5 = writeRef(byteAcc, 0) } in
            writeRef(bitNum, 0)
        else Unit in
        mandelbrotRow(x + 1, y, sz, sum, byteAcc, bitNum);

function mandelbrotFlush(sz:Int, sum:Ref(Int), byteAcc:Ref(Int), bitNum:Ref(Int)) : Unit =
    let bn = readRef(bitNum) in
    if bn == 0 then Unit
    else
        let {
            ba = readRef(byteAcc);
            shifted = shiftL(ba, 8 - bn);
            u1 = modifyRef(sum, fn(s) = xor(s, shifted));
            u2 = writeRef(byteAcc, 0);
        } in
        writeRef(bitNum, 0);

function mandelbrotRows(y:Int, sz:Int, sum:Ref(Int), byteAcc:Ref(Int), bitNum:Ref(Int)) : Unit =
    if y >= sz then Unit
    else
        let { u1 = mandelbrotRow(0, y, sz, sum, byteAcc, bitNum); u2 = mandelbrotFlush(sz, sum, byteAcc, bitNum) } in
        mandelbrotRows(y + 1, sz, sum, byteAcc, bitNum);

function mandelbrot(sz:Int) : Int =
    let { sum = newRef(0); byteAcc = newRef(0); bitNum = newRef(0); u = mandelbrotRows(0, sz, sum, byteAcc, bitNum) } in
    readRef(sum);

function benchMandelbrot() : Int = mandelbrot(500);

function verifyMandelbrot() : Bool = mandelbrot(500) == 255;

function benchIter(name:String, n:Int, i:Int, total:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let {
            start = clockNanos();
            result = benchMandelbrot();
            end = clockNanos();
            elapsed = div(end - start, 1000);
            ok = result == 255;
            u1 = if ok then Unit else putStrLn("ERROR: verification failed");
            u2 = benchOnce(name, result, elapsed, i);
            u3 = modifyRef(total, fn(t) = t + elapsed)
        } in benchIter(name, n, i + 1, total);

function main() : Unit =
    let {
        u1 = putStrLn("Starting Mandelbrot benchmark ...");
        total = newRef(0);
        u2 = benchIter("Mandelbrot", 3, 0, total);
        t = readRef(total)
    } in benchSummary("Mandelbrot", t, 3);
