/// Monadic traversal functions.
///
/// Specialized versions for the Maybe monad, since generic mapM/foldM
/// cannot dispatch pure correctly without type-directed dispatch.
///
/// @note Generic traversal requires type-directed dispatch (not yet implemented).
/// @see HKT.Monad, Core.Maybe, Core.List
module HKT.Traverse;
import HKT.Monad;
import Core.List;
import Core.Maybe;

/// Map a Maybe-returning function over a list, collecting results.
/// @param f Function returning Maybe for each element
/// @param xs The list to traverse
/// @returns Just(results) if all succeed, Nothing if any fails
/// @example mapMMaybe(\x -> Just(x+1), Cons(1, Nil)) => Just(Cons(2, Nil))
function mapMMaybe(f, xs:List(a)) = match
    | f, Nil -> Just(Nil)
    | f, Cons(h, t) -> bind(f(h), \bh -> bind(mapMMaybe(f, t), \bt -> Just(Cons(bh, bt))));

/// Fold a list with a Maybe-returning accumulator.
/// @param f Accumulation function returning Maybe
/// @param acc Initial accumulator value
/// @param xs The list to fold
/// @returns Just(finalAcc) if all steps succeed, Nothing otherwise
function foldMMaybe(f, acc, xs:List(a)) = match
    | f, acc, Nil -> Just(acc)
    | f, acc, Cons(h, t) -> bind(f(acc, h), \acc2 -> foldMMaybe(f, acc2, t));

/// Sequence a list of Maybe values into a Maybe of list.
/// @param xs A list of Maybe values
/// @returns Just(values) if all are Just, Nothing if any is Nothing
/// @example sequenceMaybe(Cons(Just(1), Cons(Just(2), Nil))) => Just(Cons(1, Cons(2, Nil)))
function sequenceMaybe(xs:List(Maybe(a))) = match
    | Nil -> Just(Nil)
    | Cons(h, t) -> bind(h, \v -> bind(sequenceMaybe(t), \vs -> Just(Cons(v, vs))));

/// mapMMaybe with arguments flipped.
/// @param xs The list to traverse
/// @param f Function returning Maybe for each element
function forMMaybe(xs:List(a), f) = mapMMaybe(f, xs);

/// Map a monadic function over a list, discarding results (for effects only).
/// @param f The effectful function
/// @param xs The list to traverse
/// @returns Unit
function mapM_(f, xs:List(a)) : Unit = match
    | f, Nil -> Unit
    | f, Cons(h, t) -> seq(f(h), mapM_(f, t));
