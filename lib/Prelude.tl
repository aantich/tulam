/// Primitive type declarations for the tulam standard library.
///
/// Prelude is loaded first, before all other modules. It declares the
/// machine-level primitive types that have no constructors and whose
/// operations are provided via intrinsic instances.
///
/// @see Numeric.Int, Numeric.Float, Numeric.String, Numeric.Char
module Prelude;

/// Platform-width signed integer (64-bit on most platforms).
/// @see Numeric.Int
primitive Int;

/// 64-bit IEEE 754 floating-point number.
/// @see Numeric.Float
primitive Float64;

/// UTF-8 encoded immutable string.
/// @see Numeric.String, Algebra.StringOps, Algebra.StringExt
primitive String;

/// Unicode scalar value (code point).
/// @see Numeric.Char
primitive Char;

/// 8-bit signed integer. Range: -128 to 127.
/// @see Numeric.Int.Fixed
primitive Int8;

/// 16-bit signed integer. Range: -32768 to 32767.
/// @see Numeric.Int.Fixed
primitive Int16;

/// 32-bit signed integer. Range: -2147483648 to 2147483647.
/// @see Numeric.Int.Fixed
primitive Int32;

/// 64-bit signed integer. Range: -9223372036854775808 to 9223372036854775807.
/// @see Numeric.Int.Fixed
primitive Int64;

/// Platform-width unsigned integer.
/// @see Numeric.UInt
primitive UInt;

/// 8-bit unsigned integer. Range: 0 to 255.
/// @see Numeric.UInt
primitive UInt8;

/// 16-bit unsigned integer. Range: 0 to 65535.
/// @see Numeric.UInt
primitive UInt16;

/// 32-bit unsigned integer. Range: 0 to 4294967295.
/// @see Numeric.UInt
primitive UInt32;

/// 64-bit unsigned integer. Range: 0 to 18446744073709551615.
/// @see Numeric.UInt
primitive UInt64;

/// 32-bit IEEE 754 floating-point number.
/// @see Numeric.Float
primitive Float32;

/// Raw unsigned byte (alias for UInt8).
primitive Byte;

/// Homogeneous, contiguous array of elements.
/// @param a Element type
/// @see Collection.Array
primitive Array(a:Type);
