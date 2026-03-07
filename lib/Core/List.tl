/// Singly-linked immutable list.
///
/// List(a) is the fundamental recursive collection type. Nil is the
/// empty list, Cons(head, tail) prepends an element.
///
/// @see HKT.Functor, HKT.Monad, Core.NonEmpty
module Core.List;

/// Singly-linked list — either empty (Nil) or head + tail (Cons).
/// @param a Element type
/// @example Nil
/// @example Cons(1, Cons(2, Nil))
type List(a:Type) = Nil | Cons(hd:a, tl:List(a));

/// Safe head — returns the first element wrapped in Maybe.
/// @param xs The list
/// @returns Just(head) or Nothing if empty
/// @example head(Nil) => Nothing
/// @example head(Cons(1, Nil)) => Just(1)
function head(xs:List(a)) : Maybe(a) = match
    | Nil -> Nothing
    | Cons(h, t) -> Just(h);

/// Safe tail — returns the rest of the list wrapped in Maybe.
/// @param xs The list
/// @returns Just(tail) or Nothing if empty
/// @example tail(Nil) => Nothing
/// @example tail(Cons(1, Cons(2, Nil))) => Just(Cons(2, Nil))
function tail(xs:List(a)) : Maybe(List(a)) = match
    | Nil -> Nothing
    | Cons(h, t) -> Just(t);

/// Check if a list is empty.
/// @param xs The list
/// @returns True if Nil, False if Cons
/// @example isEmpty(Nil) => True
/// @example isEmpty(Cons(1, Nil)) => False
function isEmpty(xs:List(a)) : Bool = match
    | Nil -> True
    | Cons(h, t) -> False;
