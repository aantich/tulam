/// String algebra instances.
///
/// Registers String for StringOps (concat, length), StringExt (charAt,
/// substring, indexOf, etc.), Eq, and Ord.
///
/// @see Algebra.StringOps, Algebra.StringExt
module Numeric.String;
import Algebra.StringOps;
import Algebra.StringExt;
import Algebra.Eq;
import Algebra.Ord;

/// String concatenation and length.
instance StringOps(String) = intrinsic;
/// String extended operations (charAt, substring, indexOf, etc.).
instance StringExt(String) = intrinsic;
/// String equality.
instance Eq(String) = intrinsic;
/// String lexicographic ordering.
instance Ord(String) = intrinsic;
