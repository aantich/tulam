/// Boolean type and basic operations.
///
/// Bool is the fundamental two-valued type for logic and branching.
/// True and False are its only inhabitants.
///
/// @see Algebra.Eq, Algebra.Ord, Algebra.Lattice
module Core.Bool;

/// The boolean type with exactly two values.
/// @example True
/// @example False
type Bool = True | False;

/// Logical negation.
/// @param b The boolean to negate
/// @returns The opposite boolean value
/// @example not(True) => False
/// @example not(False) => True
function not (b:Bool) : Bool = match
    | True  -> False
    | False -> True;
