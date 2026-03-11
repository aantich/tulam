/// List instances: Semigroup, Monoid, Show, and Collection algebras.
///
/// Also provides convenience functions: head, tail.
module Instances.List;
import Core;
import Algebra;
import Collection;

// ============================================================================
// List-specific functions
// ============================================================================

/// Safe head — returns the first element wrapped in Maybe.
function head(xs:List(a)) : Maybe(a) = match
    | Nil -> Nothing
    | Cons(h, t) -> Just(h);

/// Safe tail — returns the rest of the list wrapped in Maybe.
function tail(xs:List(a)) : Maybe(List(a)) = match
    | Nil -> Nothing
    | Cons(h, t) -> Just(t);

// ============================================================================
// Algebra instances
// ============================================================================

// ----------------------------------------------------------------------------
// Collection algebras
// ----------------------------------------------------------------------------

/// Foldable instance for List — the fundamental List eliminator.
instance Foldable(List) = {
    function foldr(f, acc, xs) = match
        | f, acc, Nil -> acc
        | f, acc, Cons(h, t) -> f(h, foldr(f, acc, t));
};

/// Sized instance for List — O(n) size via fold.
instance Sized(List) = {
    function size(xs) = foldl(fn(acc, _) = acc + 1, 0, xs);
};

/// Filterable instance for List.
instance Filterable(List) = {
    function filter(p, xs) = match
        | p, Nil -> Nil
        | p, Cons(h, t) -> if p(h) then Cons(h, filter(p, t)) else filter(p, t);
    function partition(p, xs) = match
        | p, Nil -> Pair(Nil, Nil)
        | p, Cons(h, t) ->
            let rest = partition(p, t) in
            if p(h) then Pair(Cons(h, fst(rest)), snd(rest))
            else Pair(fst(rest), Cons(h, snd(rest)));
    function filterMap(f, xs) = match
        | f, Nil -> Nil
        | f, Cons(h, t) -> match f(h)
            | Just(v) -> Cons(v, filterMap(f, t))
            | Nothing -> filterMap(f, t);
};

/// Buildable instance for List.
instance Buildable(List) = {
    function bempty() = Nil;
    function cons(x, xs) = Cons(x, xs);
    function snoc(xs, x) = match
        | Nil, x -> Cons(x, Nil)
        | Cons(h, t), x -> Cons(h, snoc(t, x));
};

// ----------------------------------------------------------------------------
// Core algebras
// ----------------------------------------------------------------------------

instance Semigroup(List(a)) = {
    function combine(xs:List(a), ys:List(a)) : List(a) = match
        | Nil, ys -> ys
        | Cons(h, t), ys -> Cons(h, combine(t, ys))
};

instance Monoid(List(a)) = {
    function combine(xs:List(a), ys:List(a)) : List(a) = match
        | Nil, ys -> ys
        | Cons(h, t), ys -> Cons(h, combine(t, ys));
    value empty = Nil
};

instance Show(List(a)) = {
    function show(x:List(a)) : String = match
        | Nil -> "Nil"
        | Cons(h, t) -> concat("Cons(", concat(show(h), concat(", ", concat(show(t), ")"))))
};
