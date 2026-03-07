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

infix 4 (<), (>), (<=), (>=);

/// Total ordering with comparison and relational operators.
/// Minimal definition: compare, OR all of (<), (>), (<=), (>=).
/// Each group has defaults in terms of the other.
algebra Ord(a:Type) extends Eq(a) = {
    /// Three-way comparison returning LessThan, Equal, or GreaterThan.
    /// Default: derived from (<) and (==).
    /// @param x Left operand
    /// @param y Right operand
    /// @returns Ordering result
    function compare(x:a, y:a) : Ordering =
        if x == y then Equal
        else if x < y then LessThan
        else GreaterThan,

    /// Strict less-than.
    /// Default: derived from compare.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x < y
    function (<)(x:a, y:a) : Bool = compare(x, y) == LessThan,

    /// Strict greater-than.
    /// Default: derived from compare.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x > y
    function (>)(x:a, y:a) : Bool = compare(x, y) == GreaterThan,

    /// Less-than-or-equal.
    /// Default: derived from compare.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x <= y
    function (<=)(x:a, y:a) : Bool = not(compare(x, y) == GreaterThan),

    /// Greater-than-or-equal.
    /// Default: derived from compare.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns True if x >= y
    function (>=)(x:a, y:a) : Bool = not(compare(x, y) == LessThan),

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
