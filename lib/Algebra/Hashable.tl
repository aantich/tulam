/// Types that can be hashed to an integer.
///
/// Hash codes are used for O(1) average-case lookup in hash-based
/// data structures. The fundamental contract: if x == y then hash(x) == hash(y).
///
/// @see Algebra.Eq
module Algebra.Hashable;
import Algebra.Eq;

/// Hashing algebra for hash-based data structures.
algebra Hashable(a:Type) = {
    /// Compute an integer hash code.
    /// @param x The value to hash
    /// @returns An integer hash code
    /// @example hash(42) => 42
    function hash(x:a) : Int,

    law consistency(x:a, y:a) = ((x == y) === True) ==> (hash(x) === hash(y)),

    /// Automatic structural hash derivation using reflection.
    /// Usage: instance Hashable(MyType) = derive;
    derive {
        function hash(x:a) : Int = structuralHash(x)
    }
};
