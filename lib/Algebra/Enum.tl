/// Types whose values can be enumerated sequentially.
///
/// Provides successor, predecessor, and Int conversion for discrete types.
///
/// @see Algebra.Bounded, Algebra.Ord
module Algebra.Enum;
import Algebra.Eq;

/// Sequential enumeration algebra.
algebra Enum(a:Type) = {
    /// Successor — the next value in the enumeration.
    /// @param x Current value
    /// @returns The next value
    /// @example succ(False) => True
    /// @example succ(0) => 1
    function succ(x:a) : a,

    /// Predecessor — the previous value in the enumeration.
    /// @param x Current value
    /// @returns The previous value
    /// @example pred(True) => False
    /// @example pred(1) => 0
    function pred(x:a) : a,

    /// Convert an integer index to the corresponding value.
    /// @param i Integer index (0-based for finite types)
    /// @returns The value at index i
    /// @example toEnum(0) : Bool => False
    function toEnum(i:Int) : a,

    /// Convert a value to its integer index.
    /// @param x The value
    /// @returns Integer index
    /// @example fromEnum(True) => 1
    function fromEnum(x:a) : Int,

    law succ_enum(x:a) = fromEnum(succ(x)) === fromEnum(x) + 1,
    law roundtrip(x:a) = toEnum(fromEnum(x)) === x
};
