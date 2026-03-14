/// Maybe instances: Show, Functor, Applicative, Monad.
///
/// Also provides convenience functions: isNothing, fromMaybe.
module Instances.Maybe;
import Core;
import Algebra;
import Categorical;

// ============================================================================
// Maybe-specific functions
// ============================================================================

/// Check if a Maybe is Nothing.
function isNothing(m:Maybe(a)) : Bool = match
    | Nothing -> True
    | Just(x) -> False;

/// Extract the value or use a default.
function fromMaybe(def:a, m:Maybe(a)) : a = match
    | d, Nothing -> d
    | d, Just(x) -> x;

// ============================================================================
// Algebra instances
// ============================================================================

instance Show(Maybe(a)) = {
    function show(x:Maybe(a)) : String = match
        | Nothing -> "Nothing"
        | Just(v) -> "Just(" ⊕ show(v) ⊕ ")"
};

// ----------------------------------------------------------------------------
// Categorical instances
// ----------------------------------------------------------------------------

instance Functor(Maybe) = {
    function fmap(g, x) = match
        | g, Nothing -> Nothing
        | g, Just(v) -> Just(g(v))
};

instance Applicative(Maybe) = {
    function pure(x) = Just(x);
    function ap(ff, fa) = match
        | Nothing, fa -> Nothing
        | Just(g), fa -> fmap(g, fa)
};

instance Monad(Maybe) = {
    function pure(x) = Just(x);
    function bind(x, f) = match
        | Nothing, f -> Nothing
        | Just(v), f -> f(v);
    function ap(ff, fa) = match
        | Nothing, fa -> Nothing
        | Just(g), fa -> fmap(g, fa);
    function seq(x, y) = match
        | Nothing, y -> Nothing
        | Just(v), y -> y
};
