/// List instances: Semigroup, Monoid, Show.
///
/// Also provides convenience functions: head, tail, isEmpty.
module Instances.List;
import Core;
import Algebra;

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

/// Check if a list is empty.
function isEmpty(xs:List(a)) : Bool = match
    | Nil -> True
    | Cons(h, t) -> False;

// ============================================================================
// Algebra instances
// ============================================================================

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
