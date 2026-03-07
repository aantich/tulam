/// Semigroup, Monoid, Group, and AbelianGroup algebraic hierarchy.
///
/// A Semigroup has an associative binary operation (combine).
/// A Monoid adds an identity element (empty).
/// A Group adds an inverse operation.
/// An AbelianGroup adds commutativity.
///
/// @see Algebra.Additive, Algebra.Multiplicative
module Algebra.Semigroup;

/// Associative binary operation.
algebra Semigroup(a:Type) = {
    /// Combine two values associatively.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns The combined value
    function combine(x:a, y:a) : a,

    law associativity(x:a, y:a, z:a) = combine(combine(x, y), z) === combine(x, combine(y, z))
};

/// Semigroup with an identity element.
algebra Monoid(a:Type) extends Semigroup(a) = {
    /// The identity element for combine.
    value empty : a,

    law left_identity(x:a) = combine(empty, x) === x,
    law right_identity(x:a) = combine(x, empty) === x
};

/// Monoid with an inverse operation.
algebra Group(a:Type) extends Monoid(a) = {
    /// Compute the inverse of a value.
    /// @param x The value to invert
    /// @returns The inverse such that combine(x, inverse(x)) === empty
    function inverse(x:a) : a,

    law left_inverse(x:a) = combine(inverse(x), x) === empty,
    law right_inverse(x:a) = combine(x, inverse(x)) === empty
};

/// Group with a commutative combine operation.
algebra AbelianGroup(a:Type) extends Group(a) = {
    law commutativity(x:a, y:a) = combine(x, y) === combine(y, x)
};
