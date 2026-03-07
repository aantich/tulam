/// Absolute value and sign decomposition.
///
/// @see Algebra.Ring, Numeric.Int
module Algebra.Absolute;

/// Absolute value and signum (sign) operations.
algebra Absolute(a:Type) = {
    /// Absolute value — the non-negative magnitude.
    /// @param x Any value
    /// @returns |x|
    /// @example abs(-5) => 5
    function abs(x:a) : a,

    /// Sign function — returns -1, 0, or 1.
    /// @param x Any value
    /// @returns The sign of x
    /// @example signum(-5) => -1
    /// @example signum(0) => 0
    /// @example signum(5) => 1
    function signum(x:a) : a,

    law decomposition(x:a) = abs(x) * signum(x) === x,
    law idempotence(x:a) = abs(abs(x)) === abs(x)
};
