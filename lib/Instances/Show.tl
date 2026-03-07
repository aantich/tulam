/// Show instances for all core and primitive types.
///
/// Each instance defines how values are rendered as strings.
/// Convention: output resembles valid tulam constructor syntax.
///
/// @see Algebra.Show
module Instances.Show;
import Algebra.Show;
import Core.Bool;
import Core.Ordering;
import Core.Nat;
import Core.Unit;
import Core.Maybe;
import Core.Either;
import Core.List;
import Core.Pair;
import Core.NonEmpty;
import Core.Tree;

// --- Primitive type instances (compiler-provided) ---

/// Show(Int): decimal integer. show(42) => "42"
instance Show(Int) = intrinsic;

/// Show(Float64): decimal float. show(3.14) => "3.14"
instance Show(Float64) = intrinsic;

/// Show(String): quoted string. show("hi") => "\"hi\""
instance Show(String) = intrinsic;

/// Show(Char): single-quoted char. show('A') => "'A'"
instance Show(Char) = intrinsic;

/// Show(Int8): decimal integer.
instance Show(Int8) = intrinsic;
/// Show(Int16): decimal integer.
instance Show(Int16) = intrinsic;
/// Show(Int32): decimal integer.
instance Show(Int32) = intrinsic;
/// Show(Int64): decimal integer.
instance Show(Int64) = intrinsic;
/// Show(UInt8): decimal unsigned integer.
instance Show(UInt8) = intrinsic;
/// Show(UInt16): decimal unsigned integer.
instance Show(UInt16) = intrinsic;
/// Show(UInt32): decimal unsigned integer.
instance Show(UInt32) = intrinsic;
/// Show(UInt64): decimal unsigned integer.
instance Show(UInt64) = intrinsic;
/// Show(Float32): decimal float.
instance Show(Float32) = intrinsic;
/// Show(Byte): decimal byte value.
instance Show(Byte) = intrinsic;

// --- Algebraic type instances ---

/// Show(Bool): show(True) => "True", show(False) => "False"
instance Show(Bool) = {
    function show(x:Bool) : String = match
        | True -> "True"
        | False -> "False"
};

/// Show(Ordering): show(LessThan) => "LessThan", etc.
instance Show(Ordering) = {
    function show(x:Ordering) : String = match
        | LessThan -> "LessThan"
        | Equal -> "Equal"
        | GreaterThan -> "GreaterThan"
};

/// Show(Unit): show(Unit) => "Unit"
instance Show(Unit) = {
    function show(x:Unit) : String = "Unit"
};

/// Show(Nat): show(Succ(Z)) => "Succ(Z)"
instance Show(Nat) = {
    function show(x:Nat) : String = match
        | Z -> "Z"
        | Succ(n) -> concat("Succ(", concat(show(n), ")"))
};

/// Show(Maybe(a)): show(Just(42)) => "Just(42)"
instance Show(Maybe(a)) = {
    function show(x:Maybe(a)) : String = match
        | Nothing -> "Nothing"
        | Just(v) -> concat("Just(", concat(show(v), ")"))
};

/// Show(Either(a,b)): show(Left(1)) => "Left(1)"
instance Show(Either(a, b)) = {
    function show(x:Either(a, b)) : String = match
        | Left(v) -> concat("Left(", concat(show(v), ")"))
        | Right(v) -> concat("Right(", concat(show(v), ")"))
};

/// Show(List(a)): show(Cons(1, Nil)) => "Cons(1, Nil)"
instance Show(List(a)) = {
    function show(x:List(a)) : String = match
        | Nil -> "Nil"
        | Cons(h, t) -> concat("Cons(", concat(show(h), concat(", ", concat(show(t), ")"))))
};

/// Show(Pair(a,b)): show(Pair(1, True)) => "Pair(1, True)"
instance Show(Pair(a, b)) = {
    function show(x:Pair(a, b)) : String = match
        | Pair(l, r) -> concat("Pair(", concat(show(l), concat(", ", concat(show(r), ")"))))
};

/// Show(NonEmpty(a)): show(NonEmpty(1, Nil)) => "NonEmpty(1, Nil)"
instance Show(NonEmpty(a)) = {
    function show(x:NonEmpty(a)) : String = match
        | NonEmpty(h, t) -> concat("NonEmpty(", concat(show(h), concat(", ", concat(show(t), ")"))))
};

/// Show(Tree(a)): show(Leaf(1)) => "Leaf(1)"
instance Show(Tree(a)) = {
    function show(x:Tree(a)) : String = match
        | Leaf(v) -> concat("Leaf(", concat(show(v), ")"))
        | Branch(l, r) -> concat("Branch(", concat(show(l), concat(", ", concat(show(r), ")"))))
};
