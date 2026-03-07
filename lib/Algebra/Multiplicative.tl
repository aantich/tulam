/// Multiplicative algebraic hierarchy: (*), one, recip, (/).
///
/// Mirrors the Semigroup/Monoid/Group hierarchy but specialized for
/// multiplication. Division is defined as multiplication by the reciprocal.
///
/// @see Algebra.Additive, Algebra.Ring
module Algebra.Multiplicative;

infixl 7 (*), (/);

/// Associative multiplication.
algebra MultiplicativeSemigroup(a:Type) = {
    /// Multiply two values.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns x * y
    function (*)(x:a, y:a) : a,

    law associativity(x:a, y:a, z:a) = (x * y) * z === x * (y * z)
};

/// Multiplication with a one element.
algebra MultiplicativeMonoid(a:Type) extends MultiplicativeSemigroup(a) = {
    /// The multiplicative identity (one).
    value one : a,

    law left_identity(x:a)  = one * x === x,
    law right_identity(x:a) = x * one === x
};

/// Multiplication with reciprocal and division.
algebra MultiplicativeGroup(a:Type) extends MultiplicativeMonoid(a) = {
    /// Multiplicative inverse (reciprocal).
    /// @param x The value to invert
    /// @returns 1/x such that x * recip(x) === one
    function recip(x:a) : a,

    /// Division (defined as multiplication by reciprocal).
    /// @param x Dividend
    /// @param y Divisor
    /// @returns x / y
    function (/)(x:a, y:a) : a = x * recip(y),

    law left_inverse(x:a)  = recip(x) * x === one,
    law right_inverse(x:a) = x * recip(x) === one,
    law division(x:a, y:a) = x / y === x * recip(y)
};
