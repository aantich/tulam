/// Unsigned integer algebra instances.
///
/// Registers UInt, UInt8-64, and Byte for arithmetic, bitwise
/// operations, equality, and ordering.
///
/// @see Numeric.Int, Algebra.Ring, Algebra.Bits
module Numeric.UInt;
import Algebra.Ring;
import Algebra.Absolute;
import Algebra.Bits;
import Algebra.Eq;
import Algebra.Ord;

// --- UInt (platform-width) ---
/// UInt arithmetic.
instance EuclideanDomain(UInt) = intrinsic;
/// UInt absolute value.
instance Absolute(UInt) = intrinsic;
/// UInt bitwise operations.
instance Bits(UInt) = intrinsic;
/// UInt equality.
instance Eq(UInt) = intrinsic;
/// UInt ordering.
instance Ord(UInt) = intrinsic;

// --- UInt8 ---
/// UInt8 arithmetic.
instance EuclideanDomain(UInt8) = intrinsic;
/// UInt8 bitwise operations.
instance Bits(UInt8) = intrinsic;
/// UInt8 equality.
instance Eq(UInt8) = intrinsic;
/// UInt8 ordering.
instance Ord(UInt8) = intrinsic;

// --- UInt16 ---
/// UInt16 arithmetic.
instance EuclideanDomain(UInt16) = intrinsic;
/// UInt16 bitwise operations.
instance Bits(UInt16) = intrinsic;
/// UInt16 equality.
instance Eq(UInt16) = intrinsic;
/// UInt16 ordering.
instance Ord(UInt16) = intrinsic;

// --- UInt32 ---
/// UInt32 arithmetic.
instance EuclideanDomain(UInt32) = intrinsic;
/// UInt32 bitwise operations.
instance Bits(UInt32) = intrinsic;
/// UInt32 equality.
instance Eq(UInt32) = intrinsic;
/// UInt32 ordering.
instance Ord(UInt32) = intrinsic;

// --- UInt64 ---
/// UInt64 arithmetic.
instance EuclideanDomain(UInt64) = intrinsic;
/// UInt64 bitwise operations.
instance Bits(UInt64) = intrinsic;
/// UInt64 equality.
instance Eq(UInt64) = intrinsic;
/// UInt64 ordering.
instance Ord(UInt64) = intrinsic;

// --- Byte ---
/// Byte arithmetic.
instance EuclideanDomain(Byte) = intrinsic;
/// Byte bitwise operations.
instance Bits(Byte) = intrinsic;
/// Byte equality.
instance Eq(Byte) = intrinsic;
/// Byte ordering.
instance Ord(Byte) = intrinsic;
