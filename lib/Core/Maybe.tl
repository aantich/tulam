/// Optional value type.
///
/// Maybe(a) represents a value that may or may not be present.
/// Nothing means absence, Just(x) means presence of x.
/// This is the safe alternative to null/nil pointers.
///
/// @see HKT.Monad, HKT.Functor
module Core.Maybe;

/// Optional value — either Nothing or Just(value).
/// @param a The type of the contained value
/// @example Nothing
/// @example Just(42)
type Maybe(a:Type) = Nothing | Just(val:a);

/// Check if a Maybe is Nothing.
/// @param m The Maybe to check
/// @returns True if Nothing, False if Just
/// @example isNothing(Nothing) => True
/// @example isNothing(Just(1)) => False
function isNothing(m:Maybe(a)) : Bool = match
    | Nothing -> True
    | Just(x) -> False;

/// Extract the value or use a default.
/// @param def Default value if Nothing
/// @param m The Maybe to unwrap
/// @returns The contained value, or def if Nothing
/// @example fromMaybe(0, Nothing) => 0
/// @example fromMaybe(0, Just(42)) => 42
function fromMaybe(def:a, m:Maybe(a)) : a = match
    | d, Nothing -> d
    | d, Just(x) -> x;
