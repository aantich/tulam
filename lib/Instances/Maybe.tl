/// Maybe instances: Show.
///
/// Also provides convenience functions: isNothing, fromMaybe.
module Instances.Maybe;
import Core;
import Algebra;

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
        | Just(v) -> concat("Just(", concat(show(v), ")"))
};
