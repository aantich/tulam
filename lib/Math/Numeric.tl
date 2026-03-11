/// Extended numeric algebras beyond Field/Floating.
///
/// Transcendental: hyperbolic, atan2, log2/log10, cbrt.
/// Special: gamma, beta, erf, bessel, incomplete gamma/beta.
/// ApproxEq: floating-point approximate comparison.
/// Conjugate: complex conjugation (identity for reals).
/// Norm: normed values with triangle inequality.
module Math.Numeric;
import Algebra;

// ============================================================================
// ApproxEq
// ============================================================================

/// Approximate equality for floating-point work.
/// Essential for numerical algorithms where exact equality is meaningless.
algebra ApproxEq(a:Type) = {
    /// Check if two values are equal within a given tolerance.
    function approxEq(x:a, y:a, tol:Float64) : Bool;

    /// Approximate equality with default tolerance (1e-10).
    function (~=)(x:a, y:a) : Bool = approxEq(x, y, 1.0e-10);

    law reflexive(x:a) = approxEq(x, x, 1.0e-10) === True;
    law symmetric(x:a, y:a, tol:Float64) = approxEq(x, y, tol) === approxEq(y, x, tol)
};

// ============================================================================
// Conjugate
// ============================================================================

/// Conjugation operation.
/// Identity for real types. Complex conjugate for complex types.
algebra Conjugate(a:Type) = {
    /// Conjugate a value.
    function conj(x:a) : a;

    law involution(x:a) = conj(conj(x)) === x
};

// ============================================================================
// Norm
// ============================================================================

/// A type with a notion of magnitude (distance from zero).
/// The norm maps to Float64 to support both integer and floating types.
algebra Norm(a:Type) requires Eq(a) = {
    /// The magnitude/absolute value as a Float64.
    function norm(x:a) : Float64;

    /// Normalize to unit magnitude. Undefined for zero.
    function normalize(x:a) : a;

    law normPositive(x:a) = norm(x) >= 0.0;
    law normTriangle(x:a, y:a) = norm(x + y) <= norm(x) + norm(y)
};

// ============================================================================
// Transcendental
// ============================================================================

/// Transcendental functions beyond what Floating provides.
/// Floating gives: sqrt, exp, log, sin, cos, tan, asin, acos, atan, pow, pi.
/// Transcendental adds: hyperbolic family, multi-base logarithms, atan2, cbrt.
algebra Transcendental(a:Type) extends Floating(a) = {
    /// Hyperbolic sine.
    function sinh(x:a) : a;
    /// Hyperbolic cosine.
    function cosh(x:a) : a;
    /// Hyperbolic tangent.
    function tanh(x:a) : a;
    /// Inverse hyperbolic sine.
    function asinh(x:a) : a;
    /// Inverse hyperbolic cosine.
    function acosh(x:a) : a;
    /// Inverse hyperbolic tangent.
    function atanh(x:a) : a;
    /// Two-argument arctangent (angle from x-axis to point (x,y)).
    function atan2(y:a, x:a) : a;
    /// Base-2 logarithm.
    function log2(x:a) : a;
    /// Base-10 logarithm.
    function log10(x:a) : a;
    /// Base-2 exponential.
    function exp2(x:a) : a;
    /// Cube root.
    function cbrt(x:a) : a;
    /// Hypotenuse: sqrt(x*x + y*y) without overflow.
    function hypot(x:a, y:a) : a;

    // Defaults via Floating
    function sinh(x:a) : a = (exp(x) - exp(negate(x))) / (one + one);
    function cosh(x:a) : a = (exp(x) + exp(negate(x))) / (one + one);
    function tanh(x:a) : a = sinh(x) / cosh(x);
    function asinh(x:a) : a = log(x + sqrt(x * x + one));
    function acosh(x:a) : a = log(x + sqrt(x * x - one));
    function atanh(x:a) : a = log((one + x) / (one - x)) / (one + one);
    function log2(x:a) : a = log(x) / log(one + one);
    function log10(x:a) : a = log(x) / log((one + one) * (one + one + one) + (one + one + one + one));
    function exp2(x:a) : a = pow(one + one, x);
    function cbrt(x:a) : a = pow(x, recip(one + one + one));
    function hypot(x:a, y:a) : a = sqrt(x * x + y * y);

    law sinhCosh(x:a) = cosh(x) * cosh(x) - sinh(x) * sinh(x) ~= one
};

// ============================================================================
// Special
// ============================================================================

/// Special mathematical functions.
/// These are the building blocks of statistical distributions, physics,
/// and engineering computations. Implementations are typically intrinsic
/// (backed by libm, Cephes, or platform math libraries).
algebra Special(a:Type) requires Floating(a) = {
    /// Gamma function: generalized factorial.
    function gamma(x:a) : a;
    /// Log-gamma: log(|gamma(x)|). Avoids overflow for large arguments.
    function lgamma(x:a) : a;
    /// Digamma (psi) function: derivative of lgamma.
    function digamma(x:a) : a;
    /// Beta function: B(x,y) = gamma(x)*gamma(y)/gamma(x+y).
    function beta(x:a, y:a) : a;
    /// Log-beta: log(B(x,y)). Numerically stable.
    function lbeta(x:a, y:a) : a;
    /// Error function.
    function erf(x:a) : a;
    /// Complementary error function: 1 - erf(x).
    function erfc(x:a) : a;
    /// Bessel function of the first kind, order n.
    function besselJ(n:Int, x:a) : a;
    /// Bessel function of the second kind, order n.
    function besselY(n:Int, x:a) : a;
    /// Modified Bessel function of the first kind, order n.
    function besselI(n:Int, x:a) : a;
    /// Modified Bessel function of the second kind, order n.
    function besselK(n:Int, x:a) : a;
    /// Lower regularized incomplete gamma function P(s,x).
    /// Critical for chi-squared distribution CDF.
    function incGamma(s:a, x:a) : a;
    /// Regularized incomplete beta function I_x(a,b).
    /// Critical for t-distribution and F-distribution CDFs.
    function incBeta(a:a, b:a, x:a) : a;

    // Defaults
    function beta(x:a, y:a) : a = exp(lbeta(x, y));
    function lbeta(x:a, y:a) : a = lgamma(x) + lgamma(y) - lgamma(x + y);
    function erfc(x:a) : a = one - erf(x)
};
