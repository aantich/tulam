/// A list guaranteed to have at least one element.
///
/// NonEmpty(a) always has a head element plus a (possibly empty) tail.
/// This enables safe head access and Semigroup without requiring Monoid.
///
/// @see Core.List, Algebra.Semigroup
module Core.NonEmpty;
import Core.List;
import Core.Nat;

/// Non-empty list — always has at least one element.
/// @param a Element type
/// @example NonEmpty(1, Nil)
/// @example NonEmpty(1, Cons(2, Nil))
type NonEmpty(a:Type) = NonEmpty(hd:a, tl:List(a));

/// Returns the first element (always safe).
/// @param xs A non-empty list
/// @returns The head element
/// @example neHead(NonEmpty(1, Nil)) => 1
function neHead(xs:NonEmpty(a)) : a = match
    | NonEmpty(h, t) -> h;

/// Converts to a regular List (forgetting the non-emptiness guarantee).
/// @param xs A non-empty list
/// @returns A regular list (always Cons, never Nil)
/// @example toList(NonEmpty(1, Nil)) => Cons(1, Nil)
function toList(xs:NonEmpty(a)) : List(a) = match
    | NonEmpty(h, t) -> Cons(h, t);

/// Creates a non-empty list with exactly one element.
/// @param x The element
/// @returns A singleton non-empty list
/// @example singleton(42) => NonEmpty(42, Nil)
function singleton(x:a) : NonEmpty(a) = NonEmpty(x, Nil);

/// Prepends an element to a non-empty list.
/// @param x The element to prepend
/// @param xs The non-empty list
/// @returns A new non-empty list with x at the front
function neCons(x:a, xs:NonEmpty(a)) : NonEmpty(a) = match
    | x, NonEmpty(h, t) -> NonEmpty(x, Cons(h, t));

/// Returns the tail (everything except the first element).
/// @param xs A non-empty list
/// @returns The tail as a regular List (may be Nil)
/// @example neTail(NonEmpty(1, Nil)) => Nil
function neTail(xs:NonEmpty(a)) : List(a) = match
    | NonEmpty(h, t) -> t;
