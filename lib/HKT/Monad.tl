/// Monad — sequencing computations with context.
///
/// Extends Applicative with bind (>>=) for chaining computations where
/// each step can depend on the result of the previous one.
///
/// @see HKT.Applicative, HKT.Functor
module HKT.Monad;
import HKT.Applicative;

/// Monad algebra for sequential computation.
algebra Monad(m:Type1) extends Applicative(m) = {
    /// Chain two computations — the second depends on the first's result.
    /// @param x The first computation
    /// @param f A function from the result of x to the next computation
    /// @returns The combined computation
    /// @example bind(Just(1), \x -> Just(x + 1)) => Just(2)
    function bind(x:m(a), f: a -> m(b)) : m(b),

    /// Sequence two computations, discarding the first result.
    /// @param x The first computation (result discarded)
    /// @param y The second computation
    /// @returns The result of y
    function seq(x:m(a), y:m(b)) : m(b),

    law left_identity(x:a, f: a -> m(b))  = bind(pure(x), f) === f(x),
    law right_identity(x:m(a)) = bind(x, pure) === x,
    law associativity(x:m(a), f: a -> m(b), g: b -> m(c)) =
        bind(bind(x, f), g) === bind(x, \y -> bind(f(y), g))
};
