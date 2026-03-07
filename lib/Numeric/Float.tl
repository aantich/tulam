/// Floating-point number algebra instances.
///
/// Registers Float64 and Float32 for Floating (transcendental functions),
/// Absolute, Eq, and Ord.
///
/// @see Algebra.Floating, Algebra.Ring
module Numeric.Float;
import Algebra.Floating;
import Algebra.Absolute;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Semigroup;

/// Float64 transcendental functions (sqrt, exp, log, sin, cos, etc.).
instance Floating(Float64) = intrinsic;
/// Float64 absolute value and signum.
instance Absolute(Float64) = intrinsic;
/// Float64 equality.
instance Eq(Float64) = intrinsic;
/// Float64 ordering.
instance Ord(Float64) = intrinsic;

/// Float64 as an abelian group under addition.
instance AbelianGroup(Float64) = {
    function combine(x:Float64, y:Float64) : Float64 = x + y,
    value empty = 0.0,
    function inverse(x:Float64) : Float64 = negate(x)
};

/// Float32 transcendental functions.
instance Floating(Float32) = intrinsic;
/// Float32 absolute value and signum.
instance Absolute(Float32) = intrinsic;
/// Float32 equality.
instance Eq(Float32) = intrinsic;
/// Float32 ordering.
instance Ord(Float32) = intrinsic;
