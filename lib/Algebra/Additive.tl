/// Additive algebraic hierarchy: (+), zero, negate, (-).
///
/// Mirrors the Semigroup/Monoid/Group hierarchy but specialized for
/// addition. Separating additive and multiplicative operations allows
/// types to have different algebraic properties for each.
///
/// @see Algebra.Multiplicative, Algebra.Ring
module Algebra.Additive;

infixl 6 (+), (-);

/// Associative addition.
algebra AdditiveSemigroup(a:Type) = {
    /// Add two values.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns x + y
    function (+)(x:a, y:a) : a,

    law associativity(x:a, y:a, z:a) = (x + y) + z === x + (y + z)
};

/// Addition with a zero element.
algebra AdditiveMonoid(a:Type) extends AdditiveSemigroup(a) = {
    /// The additive identity (zero).
    value zero : a,

    law left_identity(x:a)  = zero + x === x,
    law right_identity(x:a) = x + zero === x
};

/// Addition with negation and subtraction.
algebra AdditiveGroup(a:Type) extends AdditiveMonoid(a) = {
    /// Additive inverse.
    /// @param x The value to negate
    /// @returns -x such that x + negate(x) === zero
    function negate(x:a) : a,

    /// Subtraction (defined as addition of the negation).
    /// @param x Left operand
    /// @param y Right operand
    /// @returns x - y
    function (-)(x:a, y:a) : a = x + negate(y),

    law left_inverse(x:a)  = negate(x) + x === zero,
    law right_inverse(x:a) = x + negate(x) === zero,
    law subtraction(x:a, y:a) = x - y === x + negate(y)
};

/// Commutative addition.
algebra AdditiveAbelianGroup(a:Type) extends AdditiveGroup(a) = {
    law additive_commutativity(x:a, y:a) = x + y === y + x
};
