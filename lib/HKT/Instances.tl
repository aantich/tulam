/// Functor and Monad instances for Maybe and List.
///
/// @see HKT.Functor, HKT.Monad, Core.Maybe, Core.List
module HKT.Instances;
import HKT.Functor;
import HKT.Monad;
import Core.Maybe;
import Core.List;

/// Functor(Maybe): fmap over optional values.
instance Functor(Maybe) = {
    function fmap(g, x) = match
        | g, Nothing -> Nothing
        | g, Just(v) -> Just(g(v))
};

/// Functor(List): fmap over list elements.
instance Functor(List) = {
    function fmap(g, xs) = match
        | g, Nil -> Nil
        | g, Cons(h, t) -> Cons(g(h), fmap(g, t))
};

/// Monad(Maybe): short-circuit on Nothing.
instance Monad(Maybe) = {
    function pure(x) = Just(x),
    function bind(x, f) = match
        | Nothing, f -> Nothing
        | Just(v), f -> f(v),
    function ap(ff, fa) = match
        | Nothing, fa -> Nothing
        | Just(g), fa -> fmap(g, fa),
    function seq(x, y) = match
        | Nothing, y -> Nothing
        | Just(v), y -> y
};

/// Monad(List): list comprehension (flatMap).
instance Monad(List) = {
    function pure(x) = Cons(x, Nil),
    function bind(xs, f) = match
        | Nil, f -> Nil
        | Cons(h, t), f -> combine(f(h), bind(t, f)),
    function ap(ff, fa) = match
        | Nil, fa -> Nil
        | Cons(g, gs), fa -> combine(fmap(g, fa), ap(gs, fa)),
    function seq(xs, ys) = match
        | Nil, ys -> Nil
        | Cons(h, t), ys -> combine(ys, seq(t, ys))
};
