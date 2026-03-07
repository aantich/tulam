/// Enum instances for sequential types.
///
/// Defines succ, pred, toEnum, and fromEnum for types whose values
/// form a discrete sequence.
///
/// @see Algebra.Enum
module Instances.Enum;
import Algebra.Enum;
import Core.Bool;
import Core.Ordering;
import Core.Nat;

// --- Primitive type instances ---

/// Enum(Int): identity mapping (succ = +1, fromEnum = id).
instance Enum(Int) = intrinsic;
/// Enum(Char): Unicode code point mapping.
instance Enum(Char) = intrinsic;

// --- Algebraic type instances ---

/// Enum(Bool): False=0, True=1.
instance Enum(Bool) = {
    function succ(x:Bool) : Bool = match
        | False -> True
        | True -> True,
    function pred(x:Bool) : Bool = match
        | True -> False
        | False -> False,
    function toEnum(i:Int) : Bool = match
        | 0 -> False
        | n -> True,
    function fromEnum(x:Bool) : Int = match
        | False -> 0
        | True -> 1
};

/// Enum(Ordering): LessThan=0, Equal=1, GreaterThan=2.
instance Enum(Ordering) = {
    function succ(x:Ordering) : Ordering = match
        | LessThan -> Equal
        | Equal -> GreaterThan
        | GreaterThan -> GreaterThan,
    function pred(x:Ordering) : Ordering = match
        | GreaterThan -> Equal
        | Equal -> LessThan
        | LessThan -> LessThan,
    function toEnum(i:Int) : Ordering = match
        | 0 -> LessThan
        | 1 -> Equal
        | n -> GreaterThan,
    function fromEnum(x:Ordering) : Int = match
        | LessThan -> 0
        | Equal -> 1
        | GreaterThan -> 2
};

/// Enum(Nat): Z=0, Succ(Z)=1, Succ(Succ(Z))=2, ...
/// @note Unbounded above — there is no maxBound for Nat.
instance Enum(Nat) = {
    function succ(x:Nat) : Nat = Succ(x),
    function pred(x:Nat) : Nat = match
        | Z -> Z
        | Succ(n) -> n,
    function toEnum(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(toEnum(n - 1)),
    function fromEnum(x:Nat) : Int = match
        | Z -> 0
        | Succ(n) -> 1 + fromEnum(n)
};
