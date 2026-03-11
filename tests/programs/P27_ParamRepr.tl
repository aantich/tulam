// P27: Parameterized Repr Tests

// Simple wrapper type for testing
type PackedInts = PackedInts * arr:Array(Int);

// Helper to unwrap
function unwrapPacked(p:PackedInts) : Array(Int) = match
    | PackedInts(arr) -> arr;

// Parameterized repr: Array(Int) maps to PackedInts
repr Array(Int) as PackedInts where {
    function toRepr(a:Array(Int)) : PackedInts = PackedInts(a);
    function fromRepr(p:PackedInts) : Array(Int) = unwrapPacked(p)
};

// T1: toRepr on Array(Int) produces PackedInts
function t1() : Int =
    let p = [1, 2, 3] as PackedInts in
    size(unwrapPacked(p));

// T2: fromRepr on PackedInts produces Array(Int)
function t2() : Int =
    let arr = PackedInts([10, 20, 30]) as Array(Int) in
    index(arr, 1);

// Simple repr (backward compat): Nat as Int still works
function t3() : Int =
    let n = 0 as Nat in
    n as Int;
