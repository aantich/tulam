/// Disjoint union of two types.
///
/// Either(a, b) represents a value that is either Left(a) or Right(b).
/// By convention, Left is used for errors/failures and Right for success.
///
/// @see Core.Maybe, Morphism.Convertible
module Core.Either;

/// A value of type a (Left) or type b (Right).
/// @param a The left/error type
/// @param b The right/success type
/// @example Left("error")
/// @example Right(42)
type Either(a:Type, b:Type) = Left(val:a) | Right(val:b);
