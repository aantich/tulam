/// The type with exactly one value.
///
/// Unit is the "trivial" type, analogous to void in C/Java or () in Haskell.
/// It carries no information — there is only one possible value of type Unit.
///
/// Use cases:
///   - Return type for side-effecting functions
///   - Placeholder type parameter: Map(Key, Unit) is essentially Set(Key)
///   - Base case for recursive type-level computations
///   - The terminal object in the category of types
///
/// @see Algebra.Default, Algebra.Eq
module Core.Unit;

/// The unit type with a single constructor.
type Unit = Unit;

/// Discards any value and returns Unit.
/// @param x Any value
/// @returns Unit
/// @example unit(42) => Unit
function unit(x:a) : Unit = Unit;
