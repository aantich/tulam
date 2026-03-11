// AWFY Random — LCG pseudo-random number generator
// Used by Bounce and Storage benchmarks.
// Linear congruential generator: (seed * 1309 + 13849) .&. 65535

function randomNext(seed:Ref(Int)) : Int =
    let s = readRef(seed) in
    let ns = (s * 1309 + 13849) .&. 65535 in
    let u = writeRef(seed, ns) in
    ns;
