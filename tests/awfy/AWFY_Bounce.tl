// AWFY Bounce — Bouncing Balls Simulation
// 100 balls bouncing in a 500x500 space, counting boundary collisions.
// Uses flat MutArray(Int) for ball state: 4 ints per ball (x, y, xVel, yVel).
// Uses LCG random from AWFY_Random.

function initBalls(balls:MutArray(Int), i:Int, n:Int, seed:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let base = i * 4 in
        let r1 = randomNext(seed) in
        let u1 = mutWrite(balls, base, mod(r1, 500)) in
        let r2 = randomNext(seed) in
        let u2 = mutWrite(balls, base + 1, mod(r2, 500)) in
        let r3 = randomNext(seed) in
        let u3 = mutWrite(balls, base + 2, mod(r3, 300) - 150) in
        let r4 = randomNext(seed) in
        let u4 = mutWrite(balls, base + 3, mod(r4, 300) - 150) in
        initBalls(balls, i + 1, n, seed);

function bounceBall(balls:MutArray(Int), idx:Int) : Int =
    let base = idx * 4 in
    let x = mutRead(balls, base) in
    let y = mutRead(balls, base + 1) in
    let xv = mutRead(balls, base + 2) in
    let yv = mutRead(balls, base + 3) in
    let nx = x + xv in
    let ny = y + yv in
    let bounces = newRef(0) in
    let nxv = if nx < 0 then
            let u = modifyRef(bounces, fn(c) = c + 1) in
            0 - xv
        else if nx >= 500 then
            let u = modifyRef(bounces, fn(c) = c + 1) in
            0 - xv
        else xv in
    let nyv = if ny < 0 then
            let u = modifyRef(bounces, fn(c) = c + 1) in
            0 - yv
        else if ny >= 500 then
            let u = modifyRef(bounces, fn(c) = c + 1) in
            0 - yv
        else yv in
    let finalX = if nx < 0 then 0 - nx else if nx >= 500 then 1000 - nx else nx in
    let finalY = if ny < 0 then 0 - ny else if ny >= 500 then 1000 - ny else ny in
    let u5 = mutWrite(balls, base, finalX) in
    let u6 = mutWrite(balls, base + 1, finalY) in
    let u7 = mutWrite(balls, base + 2, nxv) in
    let u8 = mutWrite(balls, base + 3, nyv) in
    readRef(bounces);

function bounceBalls(balls:MutArray(Int), i:Int, n:Int, bounceCount:Ref(Int)) : Unit =
    if i >= n then Unit
    else
        let b = bounceBall(balls, i) in
        let u = modifyRef(bounceCount, fn(c) = c + b) in
        bounceBalls(balls, i + 1, n, bounceCount);

function simulate(balls:MutArray(Int), step:Int, steps:Int, n:Int, bounceCount:Ref(Int)) : Int =
    if step >= steps then readRef(bounceCount)
    else
        let u = bounceBalls(balls, 0, n, bounceCount) in
        simulate(balls, step + 1, steps, n, bounceCount);

function benchBounce() : Int =
    let n = 100 in
    let steps = 50 in
    let seed = newRef(74755) in
    let balls = newMutArray(n * 4, 0) in
    let u = initBalls(balls, 0, n, seed) in
    let bounceCount = newRef(0) in
    simulate(balls, 0, steps, n, bounceCount);

function verifyBounce() : Bool =
    let result = benchBounce() in
    result > 0;
