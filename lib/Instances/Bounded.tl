/// Bounded instances for finite types.
///
/// Defines minBound and maxBound for primitive numeric types and
/// algebraic types with finite ranges.
///
/// @see Algebra.Bounded
module Instances.Bounded;
import Algebra.Bounded;
import Core.Bool;
import Core.Ordering;
import Core.Unit;

// --- Primitive type instances (compiler-provided) ---

/// Bounded(Int8): -128 to 127.
instance Bounded(Int8) = intrinsic;
/// Bounded(Int16): -32768 to 32767.
instance Bounded(Int16) = intrinsic;
/// Bounded(Int32): -2147483648 to 2147483647.
instance Bounded(Int32) = intrinsic;
/// Bounded(Int64): -2^63 to 2^63-1.
instance Bounded(Int64) = intrinsic;
/// Bounded(UInt8): 0 to 255.
instance Bounded(UInt8) = intrinsic;
/// Bounded(UInt16): 0 to 65535.
instance Bounded(UInt16) = intrinsic;
/// Bounded(UInt32): 0 to 4294967295.
instance Bounded(UInt32) = intrinsic;
/// Bounded(UInt64): 0 to 2^64-1.
instance Bounded(UInt64) = intrinsic;
/// Bounded(Char): '\0' to '\x10FFFF'.
instance Bounded(Char) = intrinsic;
/// Bounded(Byte): 0 to 255.
instance Bounded(Byte) = intrinsic;

// --- Algebraic type instances ---

/// Bounded(Bool): False to True.
instance Bounded(Bool) = {
    value minBound = False,
    value maxBound = True
};

/// Bounded(Ordering): LessThan to GreaterThan.
instance Bounded(Ordering) = {
    value minBound = LessThan,
    value maxBound = GreaterThan
};

/// Bounded(Unit): Unit to Unit (trivial).
instance Bounded(Unit) = {
    value minBound = Unit,
    value maxBound = Unit
};
