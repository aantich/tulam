/// Ring hierarchy: Semiring, Ring, CommutativeRing, EuclideanDomain, Field.
///
/// Combines additive and multiplicative structures with distributivity.
/// A Ring has both addition (with negation) and multiplication.
/// A Field adds multiplicative inverses (division for all non-zero elements).
///
/// @see Algebra.Additive, Algebra.Multiplicative
module Algebra.Ring;
import Algebra.Additive;
import Algebra.Multiplicative;

/// Addition and multiplication with distributivity (no negation required).
algebra Semiring(a:Type) extends AdditiveMonoid(a), MultiplicativeMonoid(a) = {
    law left_distributivity(x:a, y:a, z:a)  = x * (y + z) === x * y + x * z,
    law right_distributivity(x:a, y:a, z:a) = (x + y) * z === x * z + y * z,
    law annihilation(x:a) = zero * x === zero,
    law additive_commutativity(x:a, y:a) = x + y === y + x
};

/// Full ring: addition (with negation) and multiplication with distributivity.
algebra Ring(a:Type) extends AdditiveAbelianGroup(a), MultiplicativeMonoid(a) = {
    law left_distributivity(x:a, y:a, z:a)  = x * (y + z) === x * y + x * z,
    law right_distributivity(x:a, y:a, z:a) = (x + y) * z === x * z + y * z,
    law annihilation(x:a) = zero * x === zero
};

/// Ring with commutative multiplication.
algebra CommutativeRing(a:Type) extends Ring(a) = {
    law multiplicative_commutativity(x:a, y:a) = x * y === y * x
};

/// Commutative ring with integer division and modulo.
algebra EuclideanDomain(a:Type) extends CommutativeRing(a) = {
    /// Integer division.
    /// @param x Dividend
    /// @param y Divisor
    /// @returns Quotient
    function div(x:a, y:a) : a,

    /// Modulo (remainder after division).
    /// @param x Dividend
    /// @param y Divisor
    /// @returns Remainder
    function mod(x:a, y:a) : a,

    law division(x:a, y:a) = div(x, y) * y + mod(x, y) === x
};

/// Commutative ring where every non-zero element has a multiplicative inverse.
algebra Field(a:Type) extends CommutativeRing(a), MultiplicativeGroup(a) = {
    law multiplicative_inverse(x:a) = x * recip(x) === one
};
