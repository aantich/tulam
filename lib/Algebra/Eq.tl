/// Equality comparison algebra.
///
/// Provides structural equality and inequality with mutual default
/// implementations — override at least one of (==) or (!=).
///
/// @see Algebra.Ord, Algebra.Hashable
module Algebra.Eq;
import Core.Bool;

/// Equality algebra with reflexivity, symmetry, and transitivity laws.
algebra Eq (a:Type) = {
  /// Test two values for equality.
  /// @param x Left operand
  /// @param y Right operand
  /// @returns True if x equals y
  /// @example 1 == 1 => True
  function (==) (x,y:a) : Bool = not (x != y),

  /// Test two values for inequality.
  /// @param x Left operand
  /// @param y Right operand
  /// @returns True if x does not equal y
  /// @example 1 != 2 => True
  function (!=) (x,y:a) : Bool = not (x == y),

  law reflexivity(x:a) = (x == x) === True,
  law symmetry(x:a, y:a) = (x == y) === (y == x),
  law transitivity(x:a, y:a, z:a) = ((x == y) === True) ==> ((y == z) === True) ==> ((x == z) === True),

  /// Automatic structural equality derivation using reflection.
  /// Usage: instance Eq(MyType) = derive;
  derive {
    function (==)(x, y:a) : Bool = structuralEq(x, y),
    function (!=)(x, y:a) : Bool = not(structuralEq(x, y))
  }
};
