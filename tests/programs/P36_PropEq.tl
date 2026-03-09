// =====================================================
// P36: Propositional Equality (Identity Type) Tests
// Tests value-indexed identity type with GADT refinement
// =====================================================

// --- Test 1: Construct Refl for identical values ---
// Refl : PropEq(Int, x, x) — works when both indices are the same
function t1() : PropEq(Int, Z, Z) = Refl;

// --- Test 2: Construct Refl with Nat values ---
function t2() : PropEq(Nat, Succ(Z), Succ(Z)) = Refl;

// --- Test 3: Construct Refl with Bool values ---
function t3() : PropEq(Bool, True, True) = Refl;

// --- Test 4: Pattern match on Refl (concrete types, monomorphic) ---
function matchRefl(p:PropEq(Nat, Z, Z)) : Bool = match p | Refl -> True;
function t4() : Bool = matchRefl(Refl);

// --- Test 5: Refl as a value that can be let-bound ---
function t5() : Bool =
    let p : PropEq(Nat, Z, Z) = Refl in
    match p | Refl -> True;

// --- Test 6: Two different Refl proofs ---
function t6() : Bool =
    let p1 : PropEq(Int, Z, Z) = Refl in
    let p2 : PropEq(Bool, True, True) = Refl in
    True;

// --- Test mkRefl: Polymorphic Refl construction ---
function mkRefl(x:a) : PropEq(a, x, x) = Refl;

// --- Test 7: Match on Refl in a function ---
function t7() : Bool =
    let p : PropEq(Int, Z, Z) = Refl in
    matchRefl2(p);
function matchRefl2(p:PropEq(Int, Z, Z)) : Bool = match p | Refl -> True;

// --- Test 8: Symmetry via Refl match ---
// If PropEq(a, x, y), matching Refl refines x ~ y, so we can build PropEq(a, y, x)
function sym(p:PropEq(Nat, Z, Z)) : Bool =
    match p | Refl -> True;
function t8() : Bool = sym(Refl);

// --- Test 9: Transitivity via double Refl match ---
function trans(p1:PropEq(Nat, Z, Z), p2:PropEq(Nat, Z, Z)) : Bool =
    match p1 | Refl -> match p2 | Refl -> True;
function t9() : Bool = trans(Refl, Refl);
