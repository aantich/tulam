/// Functor — types that can be mapped over.
///
/// A Functor provides fmap, which applies a function to every value
/// inside a container while preserving the container structure.
///
/// @see HKT.Applicative, HKT.Monad
module HKT.Functor;

/// Functor algebra for type constructors (Type1).
algebra Functor(f:Type1) = {
    /// Apply a function to every value inside the functor.
    /// @param g The function to apply
    /// @param x The functor value
    /// @returns The functor with g applied to all contained values
    /// @example fmap(Succ, Just(Z)) => Just(Succ(Z))
    function fmap(g: a -> b, x:f(a)) : f(b),

    law identity(x:f(a)) = fmap(id, x) === x,
    law composition(f:b -> c, g:a -> b, x:f(a)) =
        fmap(compose(f, g), x) === fmap(f, fmap(g, x))
};
