/// Char algebra instances.
///
/// Registers Char for equality and ordering based on Unicode code points.
///
/// @see Algebra.Eq, Algebra.Ord, Algebra.Enum
module Numeric.Char;
import Algebra.Eq;
import Algebra.Ord;

/// Char equality (Unicode code point comparison).
instance Eq(Char) = intrinsic;
/// Char ordering (Unicode code point ordering).
instance Ord(Char) = intrinsic;
