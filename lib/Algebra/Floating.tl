/// Floating-point transcendental functions.
///
/// Extends Field with sqrt, exp, log, trigonometric functions, and pi.
///
/// @see Algebra.Ring, Numeric.Float
module Algebra.Floating;
import Algebra.Ring;

/// Transcendental floating-point operations.
algebra Floating(a:Type) extends Field(a) = {
    /// Square root.
    /// @param x Non-negative value
    /// @returns The square root of x
    function sqrt(x:a) : a,

    /// Natural exponential (e^x).
    /// @param x Exponent
    /// @returns e raised to the power x
    function exp(x:a) : a,

    /// Natural logarithm (base e).
    /// @param x Positive value
    /// @returns ln(x)
    function log(x:a) : a,

    /// Sine (radians).
    function sin(x:a) : a,
    /// Cosine (radians).
    function cos(x:a) : a,
    /// Tangent (radians).
    function tan(x:a) : a,
    /// Arcsine (inverse sine).
    function asin(x:a) : a,
    /// Arccosine (inverse cosine).
    function acos(x:a) : a,
    /// Arctangent (inverse tangent).
    function atan(x:a) : a,

    /// Exponentiation (x raised to the power y).
    /// @param x Base
    /// @param y Exponent
    /// @returns x^y
    function pow(x:a, y:a) : a,

    /// The constant pi (3.14159...).
    value pi : a,

    law sqrt_square(x:a) = sqrt(x) * sqrt(x) === x,
    law exp_log(x:a) = exp(log(x)) === x,
    law pythagorean(x:a) = sin(x) * sin(x) + cos(x) * cos(x) === one
};
