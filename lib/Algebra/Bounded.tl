/// Types with a smallest and largest element.
///
/// Bounded identifies the minimum and maximum values of a type.
/// Only types with a finite range should have Bounded instances.
///
/// @see Algebra.Enum, Algebra.Ord
module Algebra.Bounded;
import Algebra.Ord;

/// Bounded range with minimum and maximum values.
algebra Bounded(a:Type) = {
    /// The smallest value of type a.
    value minBound : a,

    /// The largest value of type a.
    value maxBound : a,

    law bounds_order(x:a) = (minBound <= maxBound) === True
};
