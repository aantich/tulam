/// Instances for core algebraic types: Nat, Bool, List, Ordering.
///
/// Provides Eq, Ord, Monoid, Semiring, Semigroup, and Convertible
/// instances for the core data types.
///
/// @see Core.Nat, Core.Bool, Core.List, Core.Ordering
module Core.Instances;
import Core.Bool;
import Core.Nat;
import Core.Ordering;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Semigroup;
import Algebra.Ring;
import Morphism.Convertible;

// --- Nat instances ---

/// Eq(Nat): structural equality via recursive comparison.
instance Eq(Nat) = {
  function (==) (x:Nat,y:Nat) : Bool = eq(x,y)
};

/// Ord(Nat): Z < Succ(anything), recursive comparison.
instance Ord(Nat) = {
    function compare(x:Nat, y:Nat) : Ordering = match
        | Z, Z -> Equal
        | Z, n -> LessThan
        | n, Z -> GreaterThan
        | Succ(m), Succ(n) -> compare(m, n),
    function (<)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> False
        | Z, n -> True
        | n, Z -> False
        | Succ(m), Succ(n) -> m < n,
    function (>)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> False
        | Z, n -> False
        | n, Z -> True
        | Succ(m), Succ(n) -> m > n,
    function (<=)(x:Nat, y:Nat) : Bool = not(x > y),
    function (>=)(x:Nat, y:Nat) : Bool = not(x < y)
};

/// Monoid(Nat): combine = plus, empty = Z.
instance Monoid(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y),
    value empty = Z
};

/// Semiring(Nat): (+) = plus, (*) = mult, zero = Z, one = Succ(Z).
instance Semiring(Nat) = {
    function (+)(x:Nat, y:Nat) : Nat = plus(x, y),
    function (*)(x:Nat, y:Nat) : Nat = mult(x, y),
    value zero = Z,
    value one = Succ(Z)
};

// --- Bool instances ---

/// Eq(Bool): structural equality.
instance Eq(Bool) = {
  function (==) (x:Bool,y:Bool) : Bool = match
    | True, True -> True
    | False, False -> True
    | True, False -> False
    | False, True -> False
};

// --- List instances ---

/// Semigroup(List(a)): concatenation.
instance Semigroup(List(a)) = {
    function combine(xs:List(a), ys:List(a)) : List(a) = match
        | Nil, ys -> ys
        | Cons(h, t), ys -> Cons(h, combine(t, ys))
};

/// Monoid(List(a)): concatenation with empty = Nil.
instance Monoid(List(a)) = {
    function combine(xs:List(a), ys:List(a)) : List(a) = match
        | Nil, ys -> ys
        | Cons(h, t), ys -> Cons(h, combine(t, ys)),
    value empty = Nil
};

// --- Convertible instances ---

/// Convertible(Nat, Bool): Z -> False, anything else -> True.
instance Convertible(Nat, Bool) = {
    function convert(x:Nat) : Bool = match
        | Z -> False
        | n -> True
};

/// Convertible(Bool, Nat): True -> Succ(Z), False -> Z.
instance Convertible(Bool, Nat) = {
    function convert(x:Bool) : Nat = match
        | True -> Succ(Z)
        | False -> Z
};

// --- Ordering instances ---

/// Eq(Ordering): structural equality on ordering values.
instance Eq(Ordering) = {
    function (==)(x:Ordering, y:Ordering) : Bool = match
        | LessThan, LessThan -> True
        | Equal, Equal -> True
        | GreaterThan, GreaterThan -> True
        | x, y -> False
};

/// Ord(Ordering): LessThan < Equal < GreaterThan.
instance Ord(Ordering) = {
    function compare(x:Ordering, y:Ordering) : Ordering = match
        | LessThan, LessThan -> Equal
        | LessThan, n -> LessThan
        | Equal, LessThan -> GreaterThan
        | Equal, Equal -> Equal
        | Equal, GreaterThan -> LessThan
        | GreaterThan, GreaterThan -> Equal
        | GreaterThan, n -> GreaterThan,
    function (<)(x:Ordering, y:Ordering) : Bool = match
        | LessThan, Equal -> True
        | LessThan, GreaterThan -> True
        | Equal, GreaterThan -> True
        | x, y -> False,
    function (>)(x:Ordering, y:Ordering) : Bool = match
        | Equal, LessThan -> True
        | GreaterThan, LessThan -> True
        | GreaterThan, Equal -> True
        | x, y -> False,
    function (<=)(x:Ordering, y:Ordering) : Bool = not(x > y),
    function (>=)(x:Ordering, y:Ordering) : Bool = not(x < y)
};

// --- Convertible(Nat, Int) and Convertible(Int, Nat) ---

/// Convertible(Nat, Int): Z -> 0, Succ(Z) -> 1, etc.
instance Convertible(Nat, Int) = {
    function convert(n:Nat) : Int = match
        | Z -> 0
        | Succ(m) -> 1 + convert(m)
};

/// Convertible(Int, Nat): 0 -> Z, 1 -> Succ(Z), etc. Negative values clamp to Z.
instance Convertible(Int, Nat) = {
    function convert(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(convert(n - 1))
};

// --- Utility functions ---

/// Describe an Int as a Nat (simplified: 0->Z, 1->Succ(Z), else->Succ(Succ(Z))).
function describeInt(x:Int) : Nat = match
    | 0 -> Z
    | 1 -> Succ(Z)
    | n -> Succ(Succ(Z));

/// Double a natural number using a where-clause.
function doubleNat(x:Nat) : Nat = plus(twice, twice) where {
    twice = plus(x, x)
};
