module AWFY_NBody;
import Harness;

// AWFY NBody — N-Body Gravitational Simulation
// 5-body solar system simulation (Sun + Jupiter + Saturn + Uranus + Neptune).
// Uses flat MutArray(Float64): 7 fields per body (x, y, z, vx, vy, vz, mass).

// Body field offsets
function bX()    : Int = 0;
function bY()    : Int = 1;
function bZ()    : Int = 2;
function bVX()   : Int = 3;
function bVY()   : Int = 4;
function bVZ()   : Int = 5;
function bMASS() : Int = 6;

function bodyGet(bodies:MutArray(Float64), i:Int, field:Int) : Float64 =
    mutRead(bodies, i * 7 + field);

function bodySet(bodies:MutArray(Float64), i:Int, field:Int, val:Float64) : Unit =
    mutWrite(bodies, i * 7 + field, val);

function createBodies() : MutArray(Float64) =
    let {
        pi = 3.141592653589793;
        solarMass = 4.0 * pi * pi;
        daysPerYear = 365.24;
        b = newMutArray(35, 0.0);
        // Sun (body 0) — at origin, mass = solarMass
        u0 = bodySet(b, 0, bMASS(), solarMass);
        // Jupiter (body 1)
        u1 = bodySet(b, 1, bX(),  4.84143144246472090);
        u2 = bodySet(b, 1, bY(),  0.0 - 1.16032004402742839);
        u3 = bodySet(b, 1, bZ(),  0.0 - 0.10362204447112311);
        u4 = bodySet(b, 1, bVX(), 0.00166007664274403694 * daysPerYear);
        u5 = bodySet(b, 1, bVY(), 0.00769901118419740425 * daysPerYear);
        u6 = bodySet(b, 1, bVZ(), 0.0 - 0.00006902767869032007 * daysPerYear);
        u7 = bodySet(b, 1, bMASS(), 0.000954791938424326609 * solarMass);
        // Saturn (body 2)
        u8  = bodySet(b, 2, bX(),  8.34336671824457987);
        u9  = bodySet(b, 2, bY(),  4.12479856412430479);
        u10 = bodySet(b, 2, bZ(),  0.0 - 0.40352341711789204);
        u11 = bodySet(b, 2, bVX(), 0.0 - 0.00276742510726862411 * daysPerYear);
        u12 = bodySet(b, 2, bVY(), 0.00499852801234917238 * daysPerYear);
        u13 = bodySet(b, 2, bVZ(), 0.00023041729757376393 * daysPerYear);
        u14 = bodySet(b, 2, bMASS(), 0.000285885980666130812 * solarMass);
        // Uranus (body 3)
        u15 = bodySet(b, 3, bX(),  12.894369562139131);
        u16 = bodySet(b, 3, bY(),  0.0 - 15.111151401698631);
        u17 = bodySet(b, 3, bZ(),  0.0 - 0.22330757889265573);
        u18 = bodySet(b, 3, bVX(), 0.00296460137564761618 * daysPerYear);
        u19 = bodySet(b, 3, bVY(), 0.00237847173959480950 * daysPerYear);
        u20 = bodySet(b, 3, bVZ(), 0.0 - 0.00029612425867453170 * daysPerYear);
        u21 = bodySet(b, 3, bMASS(), 0.0000436624404335156298 * solarMass);
        // Neptune (body 4)
        u22 = bodySet(b, 4, bX(),  15.379697114850917);
        u23 = bodySet(b, 4, bY(),  0.0 - 25.919314609987964);
        u24 = bodySet(b, 4, bZ(),  0.179258772950371181);
        u25 = bodySet(b, 4, bVX(), 0.00268067772490389322 * daysPerYear);
        u26 = bodySet(b, 4, bVY(), 0.00162824170038242295 * daysPerYear);
        u27 = bodySet(b, 4, bVZ(), 0.0 - 0.00009515922545197159 * daysPerYear);
        u28 = bodySet(b, 4, bMASS(), 0.0000515138902046611451 * solarMass);
        // Offset momentum for Sun
        u29 = offsetMomentum(b, 5);
    } in b;

function offsetMomentum(bodies:MutArray(Float64), n:Int) : Unit =
    let {
        px = newRef(0.0);
        py = newRef(0.0);
        pz = newRef(0.0);
        u = offsetLoop(bodies, 0, n, px, py, pz);
        pi = 3.141592653589793;
        solarMass = 4.0 * pi * pi;
        u1 = bodySet(bodies, 0, bVX(), 0.0 - readRef(px) / solarMass);
        u2 = bodySet(bodies, 0, bVY(), 0.0 - readRef(py) / solarMass);
    } in bodySet(bodies, 0, bVZ(), 0.0 - readRef(pz) / solarMass);

function offsetLoop(bodies:MutArray(Float64), i:Int, n:Int, px:Ref(Float64), py:Ref(Float64), pz:Ref(Float64)) : Unit =
    if i >= n then Unit
    else
        let {
            m = bodyGet(bodies, i, bMASS());
            u1 = modifyRef(px, fn(v) = v + bodyGet(bodies, i, bVX()) * m);
            u2 = modifyRef(py, fn(v) = v + bodyGet(bodies, i, bVY()) * m);
            u3 = modifyRef(pz, fn(v) = v + bodyGet(bodies, i, bVZ()) * m);
        } in
        offsetLoop(bodies, i + 1, n, px, py, pz);

function advanceInner(bodies:MutArray(Float64), i:Int, j:Int, n:Int, dt:Float64) : Unit =
    if j >= n then Unit
    else
        let {
            dx = bodyGet(bodies, i, bX()) - bodyGet(bodies, j, bX());
            dy = bodyGet(bodies, i, bY()) - bodyGet(bodies, j, bY());
            dz = bodyGet(bodies, i, bZ()) - bodyGet(bodies, j, bZ());
            dist2 = dx * dx + dy * dy + dz * dz;
            dist = sqrt(dist2);
            mag = dt / (dist2 * dist);
            mj = bodyGet(bodies, j, bMASS());
            mi = bodyGet(bodies, i, bMASS());
            u1 = bodySet(bodies, i, bVX(), bodyGet(bodies, i, bVX()) - dx * mj * mag);
            u2 = bodySet(bodies, i, bVY(), bodyGet(bodies, i, bVY()) - dy * mj * mag);
            u3 = bodySet(bodies, i, bVZ(), bodyGet(bodies, i, bVZ()) - dz * mj * mag);
            u4 = bodySet(bodies, j, bVX(), bodyGet(bodies, j, bVX()) + dx * mi * mag);
            u5 = bodySet(bodies, j, bVY(), bodyGet(bodies, j, bVY()) + dy * mi * mag);
            u6 = bodySet(bodies, j, bVZ(), bodyGet(bodies, j, bVZ()) + dz * mi * mag);
        } in
        advanceInner(bodies, i, j + 1, n, dt);

function advanceOuter(bodies:MutArray(Float64), i:Int, n:Int, dt:Float64) : Unit =
    if i >= n then Unit
    else
        let u = advanceInner(bodies, i, i + 1, n, dt) in
        advanceOuter(bodies, i + 1, n, dt);

function advancePositions(bodies:MutArray(Float64), i:Int, n:Int, dt:Float64) : Unit =
    if i >= n then Unit
    else
        let {
            u1 = bodySet(bodies, i, bX(), bodyGet(bodies, i, bX()) + dt * bodyGet(bodies, i, bVX()));
            u2 = bodySet(bodies, i, bY(), bodyGet(bodies, i, bY()) + dt * bodyGet(bodies, i, bVY()));
            u3 = bodySet(bodies, i, bZ(), bodyGet(bodies, i, bZ()) + dt * bodyGet(bodies, i, bVZ()));
        } in
        advancePositions(bodies, i + 1, n, dt);

function advance(bodies:MutArray(Float64), dt:Float64, n:Int) : Unit =
    let u1 = advanceOuter(bodies, 0, n, dt) in
    advancePositions(bodies, 0, n, dt);

function energyInner(bodies:MutArray(Float64), i:Int, j:Int, n:Int, e:Ref(Float64)) : Unit =
    if j >= n then Unit
    else
        let {
            dx = bodyGet(bodies, i, bX()) - bodyGet(bodies, j, bX());
            dy = bodyGet(bodies, i, bY()) - bodyGet(bodies, j, bY());
            dz = bodyGet(bodies, i, bZ()) - bodyGet(bodies, j, bZ());
            dist = sqrt(dx * dx + dy * dy + dz * dz);
            mi = bodyGet(bodies, i, bMASS());
            mj = bodyGet(bodies, j, bMASS());
            u = modifyRef(e, fn(v) = v - mi * mj / dist);
        } in
        energyInner(bodies, i, j + 1, n, e);

function energyOuter(bodies:MutArray(Float64), i:Int, n:Int, e:Ref(Float64)) : Unit =
    if i >= n then Unit
    else
        let {
            vx = bodyGet(bodies, i, bVX());
            vy = bodyGet(bodies, i, bVY());
            vz = bodyGet(bodies, i, bVZ());
            m = bodyGet(bodies, i, bMASS());
            u1 = modifyRef(e, fn(v) = v + 0.5 * m * (vx * vx + vy * vy + vz * vz));
            u2 = energyInner(bodies, i, i + 1, n, e);
        } in
        energyOuter(bodies, i + 1, n, e);

function energy(bodies:MutArray(Float64), n:Int) : Float64 =
    let { e = newRef(0.0); u = energyOuter(bodies, 0, n, e) } in
    readRef(e);

function advanceN(bodies:MutArray(Float64), dt:Float64, n:Int, steps:Int, step:Int) : Unit =
    if step >= steps then Unit
    else
        let u = advance(bodies, dt, n) in
        advanceN(bodies, dt, n, steps, step + 1);

function benchNBody() : Float64 =
    let { bodies = createBodies(); n = 5; u = advanceN(bodies, 0.01, n, 250000, 0) } in
    energy(bodies, n);

function verifyNBody() : Bool =
    let e = benchNBody() in
    // Energy should be close to -0.1690859889909308
    e < 0.0 - 0.1;

function benchIter(name:String, n:Int, i:Int, total:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let {
            start = clockNanos();
            result = benchNBody();
            end = clockNanos();
            elapsed = div(end - start, 1000);
            ok = result < 0.0 - 0.1;
            u1 = if ok then Unit else putStrLn("ERROR: verification failed");
            u2 = benchOnce(name, result, elapsed, i);
            u3 = modifyRef(total, fn(t) = t + elapsed)
        } in benchIter(name, n, i + 1, total);

function main() : Unit =
    let {
        u1 = putStrLn("Starting NBody benchmark ...");
        total = newRef(0);
        u2 = benchIter("NBody", 3, 0, total);
        t = readRef(total)
    } in benchSummary("NBody", t, 3);
