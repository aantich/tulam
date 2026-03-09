/// Nat instances: Eq, Ord, Show, Enum, Hashable, Semigroup, Monoid, Semiring.
///
/// All Nat operations (plus, mult, eq, natMax) are defined here as algebra
/// instances or helper functions, not as standalone Core functions.
module Instances.Nat;
import Core;
import Algebra;
import Instances.Bool;
import Instances.Ordering;

// ============================================================================
// Nat helper functions
// ============================================================================

/// Addition of natural numbers.
function plus(x:Nat, y:Nat) : Nat = match
    | Z, n -> n
    | Succ(n), m -> Succ(plus(n, m));

/// Multiplication of natural numbers.
function mult(x:Nat, y:Nat) : Nat = match
    | Z, n -> Z
    | Succ(n), m -> plus(mult(n, m), m);

/// Returns the larger of two natural numbers.
function natMax(x:Nat, y:Nat) : Nat = match
    | Z, n -> n
    | n, Z -> n
    | Succ(m), Succ(n) -> Succ(natMax(m, n));

// ============================================================================
// Algebra instances
// ============================================================================

instance Eq(Nat) = {
    function (==)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> True
        | Z, n -> False
        | n, Z -> False
        | Succ(m), Succ(n) -> m == n
};

instance Ord(Nat) = {
    function compare(x:Nat, y:Nat) : Ordering = match
        | Z, Z -> Equal
        | Z, n -> LessThan
        | n, Z -> GreaterThan
        | Succ(m), Succ(n) -> compare(m, n);
    function (<)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> False
        | Z, n -> True
        | n, Z -> False
        | Succ(m), Succ(n) -> m < n;
    function (>)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> False
        | Z, n -> False
        | n, Z -> True
        | Succ(m), Succ(n) -> m > n;
    function (<=)(x:Nat, y:Nat) : Bool = not(x > y);
    function (>=)(x:Nat, y:Nat) : Bool = not(x < y)
};

instance Show(Nat) = {
    function show(x:Nat) : String = match
        | Z -> "Z"
        | Succ(n) -> concat("Succ(", concat(show(n), ")"))
};

instance Enum(Nat) = {
    function succ(x:Nat) : Nat = Succ(x);
    function pred(x:Nat) : Nat = match
        | Z -> Z
        | Succ(n) -> n;
    function toEnum(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(toEnum(n - 1));
    function fromEnum(x:Nat) : Int = match
        | Z -> 0
        | Succ(n) -> 1 + fromEnum(n)
};

instance Hashable(Nat) = {
    function hash(x:Nat) : Int = match
        | Z -> 0
        | Succ(n) -> 1 + hash(n)
};

instance Semigroup(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y)
};

instance Monoid(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y);
    value empty = Z
};

instance Semiring(Nat) = {
    function (+)(x:Nat, y:Nat) : Nat = plus(x, y);
    function (*)(x:Nat, y:Nat) : Nat = mult(x, y);
    value zero = Z;
    value one = Succ(Z)
};
