/// Pair instances: Eq, Show.
///
/// Also provides projection and transformation functions: fst, snd, swap,
/// mapFst, mapSnd, both.
module Instances.Pair;
import Core;
import Algebra;

// ============================================================================
// Pair-specific functions
// ============================================================================

/// First projection.
function fst(p:Pair(a, b)) : a = match
    | Pair(x, y) -> x;

/// Second projection.
function snd(p:Pair(a, b)) : b = match
    | Pair(x, y) -> y;

/// Exchanges the two components.
function swap(p:Pair(a, b)) : Pair(b, a) = match
    | Pair(x, y) -> Pair(y, x);

/// Applies a function to the first component.
function mapFst(f:(a -> c), p:Pair(a, b)) : Pair(c, b) = match
    | f, Pair(x, y) -> Pair(f(x), y);

/// Applies a function to the second component.
function mapSnd(f:(b -> c), p:Pair(a, b)) : Pair(a, c) = match
    | f, Pair(x, y) -> Pair(x, f(y));

/// Applies the same function to both components of a homogeneous pair.
function both(f:(a -> b), p:Pair(a, a)) : Pair(b, b) = match
    | f, Pair(x, y) -> Pair(f(x), f(y));

// ============================================================================
// Algebra instances
// ============================================================================

instance Eq(Pair(a, b)) = {
    function (==)(x:Pair(a, b), y:Pair(a, b)) : Bool = match
        | Pair(x1, x2), Pair(y1, y2) ->
            if x1 == y1 then x2 == y2 else False
};

instance Show(Pair(a, b)) = {
    function show(x:Pair(a, b)) : String = match
        | Pair(l, r) -> "Pair(" ⊕ show(l) ⊕ ", " ⊕ show(r) ⊕ ")"
};
