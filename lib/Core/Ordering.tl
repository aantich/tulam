/// Three-valued comparison result type.
///
/// Ordering represents the result of comparing two values.
/// Used as the return type of `compare` in the Ord algebra.
///
/// @see Algebra.Ord
module Core.Ordering;

/// Result of comparing two values.
/// @example compare(1, 2) => LessThan
/// @example compare(2, 2) => Equal
/// @example compare(3, 2) => GreaterThan
type Ordering = LessThan | Equal | GreaterThan;
