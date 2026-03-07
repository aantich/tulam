/// Platform-width signed integer algebra instances.
///
/// Registers Int for EuclideanDomain (arithmetic), Absolute, Bits,
/// Eq, Ord, and Group via intrinsic implementations.
///
/// @see Algebra.Ring, Algebra.Bits, Numeric.Int.Fixed
module Numeric.Int;
import Algebra.Ring;
import Algebra.Absolute;
import Algebra.Bits;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Semigroup;

/// Int arithmetic: (+), (-), (*), negate, zero, one, div, mod.
instance EuclideanDomain(Int) = intrinsic;
/// Int absolute value and signum.
instance Absolute(Int) = intrinsic;
/// Int bitwise operations.
instance Bits(Int) = intrinsic;
/// Int equality.
instance Eq(Int) = intrinsic;
/// Int ordering.
instance Ord(Int) = intrinsic;

/// Int as an additive group (combine = +, empty = 0, inverse = negate).
instance Group(Int) = {
    function combine(x:Int, y:Int) : Int = x + y,
    value empty = 0,
    function inverse(x:Int) : Int = negate(x)
};
