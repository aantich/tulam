module AWFY_Bounce;
import AWFY_Random;
import Harness;

// AWFY Bounce — Bouncing Balls Simulation
// 100 balls bouncing in a 500x500 space, counting boundary collisions.
// Uses flat MutArray(Int) for ball state: 4 ints per ball (x, y, xVel, yVel).

function initBalls(balls:MutArray(Int), i:Int, n:Int, seed:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let {
            base = i * 4;
            r1 = randomNext(seed);
            u1 = mutWrite(balls, base, mod(r1, 500));
            r2 = randomNext(seed);
            u2 = mutWrite(balls, base + 1, mod(r2, 500));
            r3 = randomNext(seed);
            u3 = mutWrite(balls, base + 2, mod(r3, 300) - 150);
            r4 = randomNext(seed);
            u4 = mutWrite(balls, base + 3, mod(r4, 300) - 150);
        } in
        initBalls(balls, i + 1, n, seed);

// bounceWall: pure function, returns (clampedPos, newVel, didBounce) packed as 3 values.
// We encode the triple as: pos * 1000000 + vel_encoded * 10 + bounce_flag
// where vel_encoded = vel + 500000 (to handle negatives), bounce_flag = 0 or 1.
// Actually simpler: just return each component separately via helper functions.

function bounceWallPos(pos:Int, vel:Int, limit:Int) : Int =
    if pos > limit then limit
    else if pos < 0 then 0
    else pos;

function bounceWallVel(pos:Int, vel:Int, limit:Int) : Int =
    if pos > limit then 0 - abs(vel)
    else if pos < 0 then abs(vel)
    else vel;

function bounceWallHit(pos:Int, limit:Int) : Bool =
    if pos > limit then True
    else if pos < 0 then True
    else False;

function abs(x:Int) : Int = if x < 0 then 0 - x else x;

function bounceBall(balls:MutArray(Int), idx:Int) : Int =
    let {
        base = idx * 4;
        x = mutRead(balls, base);
        y = mutRead(balls, base + 1);
        xv = mutRead(balls, base + 2);
        yv = mutRead(balls, base + 3);
        nx = x + xv;
        ny = y + yv;
        // Pure computation — no Ref needed
        nxv = bounceWallVel(nx, xv, 500);
        nyv = bounceWallVel(ny, yv, 500);
        finalX = bounceWallPos(nx, xv, 500);
        finalY = bounceWallPos(ny, yv, 500);
        bx = bounceWallHit(nx, 500);
        by = bounceWallHit(ny, 500);
        bounced = if bx then 1 else if by then 1 else 0;
        u1 = mutWrite(balls, base, finalX);
        u2 = mutWrite(balls, base + 1, finalY);
        u3 = mutWrite(balls, base + 2, nxv);
        u4 = mutWrite(balls, base + 3, nyv);
    } in bounced;

function bounceBalls(balls:MutArray(Int), i:Int, n:Int, bounceCount:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let { b = bounceBall(balls, i); u = modifyRef(bounceCount, fn(c) = c + b) } in
        bounceBalls(balls, i + 1, n, bounceCount);

function simulate(balls:MutArray(Int), step:Int, steps:Int, n:Int, bounceCount:Ref(Int)) : Int =
    if step >= steps then readRef(bounceCount)
    else
        let u = bounceBalls(balls, 0, n, bounceCount) in
        simulate(balls, step + 1, steps, n, bounceCount);

function benchBounce() : Int =
    let {
        n = 100;
        steps = 50;
        seed = newRef(74755);
        balls = newMutArray(n * 4, 0);
        u = initBalls(balls, 0, n, seed);
        bounceCount = newRef(0);
    } in
    simulate(balls, 0, steps, n, bounceCount);

function verifyBounce() : Bool =
    let result = benchBounce() in
    result > 0;

function benchIter(name:String, n:Int, i:Int, total:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let {
            start = clockNanos();
            result = benchBounce();
            end = clockNanos();
            elapsed = div(end - start, 1000);
            ok = result > 0;
            u1 = if ok then Unit else putStrLn("ERROR: verification failed");
            u2 = benchOnce(name, result, elapsed, i);
            u3 = modifyRef(total, fn(t) = t + elapsed)
        } in benchIter(name, n, i + 1, total);

function main() : Unit =
    let {
        u1 = putStrLn("Starting Bounce benchmark ...");
        total = newRef(0);
        u2 = benchIter("Bounce", 10, 0, total);
        t = readRef(total)
    } in benchSummary("Bounce", t, 10);
