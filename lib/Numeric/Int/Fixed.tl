/// Fixed-width signed integer algebra instances.
///
/// Registers Int8, Int16, Int32, and Int64 for arithmetic, absolute
/// value, bitwise operations, equality, and ordering.
///
/// @see Numeric.Int, Algebra.Ring
module Numeric.Int.Fixed;
import Algebra.Ring;
import Algebra.Absolute;
import Algebra.Bits;
import Algebra.Eq;
import Algebra.Ord;

// --- Int8 ---
/// Int8 arithmetic.
instance EuclideanDomain(Int8) = intrinsic;
/// Int8 absolute value and signum.
instance Absolute(Int8) = intrinsic;
/// Int8 bitwise operations.
instance Bits(Int8) = intrinsic;
/// Int8 equality.
instance Eq(Int8) = intrinsic;
/// Int8 ordering.
instance Ord(Int8) = intrinsic;

// --- Int16 ---
/// Int16 arithmetic.
instance EuclideanDomain(Int16) = intrinsic;
/// Int16 absolute value and signum.
instance Absolute(Int16) = intrinsic;
/// Int16 bitwise operations.
instance Bits(Int16) = intrinsic;
/// Int16 equality.
instance Eq(Int16) = intrinsic;
/// Int16 ordering.
instance Ord(Int16) = intrinsic;

// --- Int32 ---
/// Int32 arithmetic.
instance EuclideanDomain(Int32) = intrinsic;
/// Int32 absolute value and signum.
instance Absolute(Int32) = intrinsic;
/// Int32 bitwise operations.
instance Bits(Int32) = intrinsic;
/// Int32 equality.
instance Eq(Int32) = intrinsic;
/// Int32 ordering.
instance Ord(Int32) = intrinsic;

// --- Int64 ---
/// Int64 arithmetic.
instance EuclideanDomain(Int64) = intrinsic;
/// Int64 absolute value and signum.
instance Absolute(Int64) = intrinsic;
/// Int64 bitwise operations.
instance Bits(Int64) = intrinsic;
/// Int64 equality.
instance Eq(Int64) = intrinsic;
/// Int64 ordering.
instance Ord(Int64) = intrinsic;
