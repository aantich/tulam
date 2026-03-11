/// NonEmpty instances: Semigroup, Show.
///
/// Also provides convenience functions: neHead, toList, singleton, neCons, neTail.
module Instances.NonEmpty;
import Core;
import Algebra;

// ============================================================================
// NonEmpty-specific functions
// ============================================================================

/// Returns the first element (always safe).
function neHead(xs:NonEmpty(a)) : a = match
    | NonEmpty(h, t) -> h;

/// Converts to a regular List.
function toList(xs:NonEmpty(a)) : List(a) = match
    | NonEmpty(h, t) -> Cons(h, t);

/// Creates a non-empty list with exactly one element.
function singleton(x:a) : NonEmpty(a) = NonEmpty(x, Nil);

/// Prepends an element to a non-empty list.
function neCons(x:a, xs:NonEmpty(a)) : NonEmpty(a) = match
    | x, NonEmpty(h, t) -> NonEmpty(x, Cons(h, t));

/// Returns the tail as a regular List.
function neTail(xs:NonEmpty(a)) : List(a) = match
    | NonEmpty(h, t) -> t;

// ============================================================================
// Algebra instances
// ============================================================================

instance Semigroup(NonEmpty(a)) = {
    function combine(xs:NonEmpty(a), ys:NonEmpty(a)) : NonEmpty(a) = match
        | NonEmpty(h, t), ys -> NonEmpty(h, listCombine(t, toList(ys)));
};

/// Helper: concatenate two lists.
function listCombine(xs:List(a), ys:List(a)) : List(a) = match
    | Nil, ys -> ys
    | Cons(h, t), ys -> Cons(h, listCombine(t, ys));

instance Show(NonEmpty(a)) = {
    function show(x:NonEmpty(a)) : String = match
        | NonEmpty(h, t) -> concat("NonEmpty(", concat(show(h), concat(", ", concat(show(t), ")"))))
};
