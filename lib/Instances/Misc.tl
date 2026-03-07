/// Miscellaneous instances: Default, Hashable, Lattice, Eq, Ord, Semigroup.
///
/// Collects instances that don't fit neatly into other instance modules.
///
/// @see Algebra.Default, Algebra.Hashable, Algebra.Lattice
module Instances.Misc;
import Algebra.Default;
import Algebra.Hashable;
import Algebra.Lattice;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Semigroup;
import Core.Bool;
import Core.Ordering;
import Core.Nat;
import Core.Unit;
import Core.Maybe;
import Core.Either;
import Core.List;
import Core.Pair;
import Core.NonEmpty;

// --- Default instances ---

/// Default(Int): 0
instance Default(Int) = {
    value default = 0
};

/// Default(Float64): 0.0
instance Default(Float64) = {
    value default = 0.0
};

/// Default(Bool): False
instance Default(Bool) = {
    value default = False
};

/// Default(Nat): Z
instance Default(Nat) = {
    value default = Z
};

/// Default(String): ""
instance Default(String) = {
    value default = ""
};

/// Default(Unit): Unit
instance Default(Unit) = {
    value default = Unit
};

/// Default(List(a)): Nil
instance Default(List(a)) = {
    value default = Nil
};

/// Default(Maybe(a)): Nothing
instance Default(Maybe(a)) = {
    value default = Nothing
};

// --- Hashable instances ---

/// Hashable(Int): identity hash.
instance Hashable(Int) = intrinsic;

/// Hashable(String): FNV-1a string hash.
instance Hashable(String) = intrinsic;

/// Hashable(Char): Unicode code point hash.
instance Hashable(Char) = intrinsic;

/// Hashable(Bool): True=1, False=0.
instance Hashable(Bool) = {
    function hash(x:Bool) : Int = match
        | True -> 1
        | False -> 0
};

/// Hashable(Unit): always 0.
instance Hashable(Unit) = {
    function hash(x:Unit) : Int = 0
};

/// Hashable(Ordering): LessThan=0, Equal=1, GreaterThan=2.
instance Hashable(Ordering) = {
    function hash(x:Ordering) : Int = match
        | LessThan -> 0
        | Equal -> 1
        | GreaterThan -> 2
};

/// Hashable(Nat): hash by counting successors.
instance Hashable(Nat) = {
    function hash(x:Nat) : Int = match
        | Z -> 0
        | Succ(n) -> 1 + hash(n)
};

// --- Lattice instances (Bool) ---

/// Bool join: logical OR.
instance JoinSemiLattice(Bool) = {
    function (\/)(x:Bool, y:Bool) : Bool = match
        | False, False -> False
        | x, y -> True
};

/// Bool meet: logical AND.
instance MeetSemiLattice(Bool) = {
    function (/\)(x:Bool, y:Bool) : Bool = match
        | True, True -> True
        | x, y -> False
};

/// Bool lattice (join + meet with absorption).
instance Lattice(Bool) = {};

/// Bool bounded lattice: bottom=False, top=True.
instance BoundedLattice(Bool) = {
    value bottom = False,
    value top = True
};

// --- Eq and Ord instances ---

/// Eq(Unit): all Unit values are equal.
instance Eq(Unit) = {
    function (==)(x:Unit, y:Unit) : Bool = True,
    function (!=)(x:Unit, y:Unit) : Bool = False
};

/// Ord(Unit): trivial — all Units compare as Equal.
instance Ord(Unit) = {
    function compare(x:Unit, y:Unit) : Ordering = Equal,
    function (<)(x:Unit, y:Unit) : Bool = False,
    function (>)(x:Unit, y:Unit) : Bool = False,
    function (<=)(x:Unit, y:Unit) : Bool = True,
    function (>=)(x:Unit, y:Unit) : Bool = True
};

/// Eq(Pair(a,b)): component-wise equality.
instance Eq(Pair(a, b)) = {
    function (==)(x:Pair(a, b), y:Pair(a, b)) : Bool = match
        | Pair(x1, x2), Pair(y1, y2) -> match
            | True -> x2 == y2
            | False -> False
};

// --- Semigroup/Monoid instances ---

/// Semigroup(NonEmpty(a)): concatenation.
instance Semigroup(NonEmpty(a)) = {
    function combine(xs:NonEmpty(a), ys:NonEmpty(a)) : NonEmpty(a) = match
        | NonEmpty(h, Nil), ys -> NonEmpty(h, toList(ys))
        | NonEmpty(h, Cons(t, rest)), ys -> NonEmpty(h, Cons(t, combine(rest, toList(ys))))
};

/// Semigroup(Unit): trivial.
instance Semigroup(Unit) = {
    function combine(x:Unit, y:Unit) : Unit = Unit
};

/// Monoid(Unit): trivial monoid.
instance Monoid(Unit) = {
    function combine(x:Unit, y:Unit) : Unit = Unit,
    value empty = Unit
};
