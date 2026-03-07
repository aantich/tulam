/// Numeric type conversion instances.
///
/// Provides widening (safe) and narrowing (may truncate) conversions
/// between numeric types via the Convertible morphism.
///
/// @see Morphism.Convertible
module Numeric.Conversion;
import Morphism.Convertible;

// --- Widening conversions (safe, no data loss) ---
/// Int8 -> Int16 (safe widening).
instance Convertible(Int8, Int16) = intrinsic;
/// Int16 -> Int32 (safe widening).
instance Convertible(Int16, Int32) = intrinsic;
/// Int32 -> Int64 (safe widening).
instance Convertible(Int32, Int64) = intrinsic;
/// Int32 -> Int (safe widening).
instance Convertible(Int32, Int) = intrinsic;
/// UInt8 -> UInt16 (safe widening).
instance Convertible(UInt8, UInt16) = intrinsic;
/// UInt16 -> UInt32 (safe widening).
instance Convertible(UInt16, UInt32) = intrinsic;
/// UInt32 -> UInt64 (safe widening).
instance Convertible(UInt32, UInt64) = intrinsic;
/// Float32 -> Float64 (safe widening).
instance Convertible(Float32, Float64) = intrinsic;
/// Int -> Float64 (safe for values within Float64 precision).
instance Convertible(Int, Float64) = intrinsic;

// --- Narrowing conversions (may truncate) ---
/// Int -> Int32 (may truncate).
instance Convertible(Int, Int32) = intrinsic;
/// Int32 -> Int16 (may truncate).
instance Convertible(Int32, Int16) = intrinsic;
/// Int16 -> Int8 (may truncate).
instance Convertible(Int16, Int8) = intrinsic;
/// Float64 -> Float32 (may lose precision).
instance Convertible(Float64, Float32) = intrinsic;
/// Int -> UInt (reinterprets sign bit).
instance Convertible(Int, UInt) = intrinsic;
/// UInt -> Int (reinterprets sign bit).
instance Convertible(UInt, Int) = intrinsic;
