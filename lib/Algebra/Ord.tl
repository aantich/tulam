/// Total ordering algebra.
///
/// Extends Eq with comparison operations. Provides compare, (<), (>),
/// (<=), and (>=) for types with a natural total order.
///
/// @see Algebra.Eq, Core.Ordering
module Algebra.Ord;
import Core.Bool;
import Core.Ordering;
import Algebra.Eq;

/// Total ordering with comparison and relational operators.
algebra Ord(a:Type) extends Eq(a) = {
    /// Three-way comparison returning LessThan, Equal, or GreaterThan.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns Ordering result
    function compare(x:a, y:a) : Ordering,

    /// Strict less-than.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x < y
    function (<)(x:a, y:a) : Bool,

    /// Strict greater-than.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x > y
    function (>)(x:a, y:a) : Bool,

    /// Less-than-or-equal.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x <= y
    function (<=)(x:a, y:a) : Bool,

    /// Greater-than-or-equal.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x >= y
    function (>=)(x:a, y:a) : Bool,

    law trichotomy(x:a, y:a) = (x < y) === not(x >= y),
    law antisymmetry(x:a, y:a) = ((x <= y) === True) ==> ((y <= x) === (x == y)),
    law transitivity(x:a, y:a, z:a) = ((x <= y) === True) ==> ((y <= z) === True) ==> ((x <= z) === True),

    /// Automatic structural ordering derivation using reflection.
    /// Usage: instance Ord(MyType) = derive;
    derive {
        function compare(x, y:a) : Ordering = structuralCompare(x, y),
        function (<)(x, y:a) : Bool = match structuralCompare(x, y) | LessThan -> True | _ -> False,
        function (>)(x, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> True | _ -> False,
        function (<=)(x, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> False | _ -> True,
        function (>=)(x, y:a) : Bool = match structuralCompare(x, y) | LessThan -> False | _ -> True
    }
};
