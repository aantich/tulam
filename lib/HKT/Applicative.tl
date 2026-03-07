/// Applicative functor — functors with pure and apply.
///
/// Extends Functor with the ability to lift values into the functor (pure)
/// and apply wrapped functions to wrapped values (ap).
///
/// @see HKT.Functor, HKT.Monad
module HKT.Applicative;
import HKT.Functor;

/// Applicative functor algebra.
algebra Applicative(f:Type1) extends Functor(f) = {
    /// Lift a value into the functor.
    /// @param x The value to lift
    /// @returns The value wrapped in the functor
    /// @example pure(1) : Maybe(Int) => Just(1)
    function pure(x:a) : f(a),

    /// Apply a wrapped function to a wrapped value.
    /// @param ff Functor containing a function
    /// @param fa Functor containing a value
    /// @returns The result of applying the function inside the functor
    function ap(ff:f(a -> b), fa:f(a)) : f(b)
};
