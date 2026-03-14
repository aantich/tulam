/// Monadic helper functions (Maybe-specialized).
///
/// These are convenience functions for common monadic traversal patterns
/// specialized to Maybe. General monadic traversal requires rank-2 types.
module Instances.Categorical;
import Core;
import Categorical;

/// Map a Maybe-returning function over a list, collecting results.
/// Short-circuits on the first Nothing.
function mapMMaybe(f, xs:List(a)) = match
    | f, Nil -> Just(Nil)
    | f, Cons(h, t) -> bind(f(h), fn(bh) = bind(mapMMaybe(f, t), fn(bt) = Just(Cons(bh, bt))));

/// Fold a list with a Maybe-returning accumulator.
function foldMMaybe(f, acc, xs:List(a)) = match
    | f, acc, Nil -> Just(acc)
    | f, acc, Cons(h, t) -> bind(f(acc, h), fn(acc2) = foldMMaybe(f, acc2, t));

/// Sequence a list of Maybe values into a Maybe of list.
function sequenceMaybe(xs:List(Maybe(a))) = match
    | Nil -> Just(Nil)
    | Cons(h, t) -> bind(h, fn(v) = bind(sequenceMaybe(t), fn(vs) = Just(Cons(v, vs))));

/// mapMMaybe with arguments flipped.
function forMMaybe(xs:List(a), f) = mapMMaybe(f, xs);

/// Map a monadic function over a list, discarding results.
function mapM_(f, xs:List(a)) = match
    | f, Nil -> pure(Unit)
    | f, Cons(h, t) -> seq(f(h), mapM_(f, t));
