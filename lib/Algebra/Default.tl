/// Types with a canonical "zero" or "empty" value.
///
/// Default provides a single distinguished value — the "obvious" starting
/// point for a type. Unlike Monoid, Default has no laws.
///
/// @see Algebra.Semigroup
module Algebra.Default;

/// Canonical default value algebra.
algebra Default(a:Type) = {
    /// The default value of type a.
    /// @returns The canonical "zero" or "empty" value
    value default : a
};
