/// SIMD vector primitive types.
///
/// Fixed-width vector types for SIMD (Single Instruction, Multiple Data)
/// parallel computation. Operations provided via the Lane algebra.
///
/// @see SIMD.Lane
module SIMD.Vec;

/// 2-lane SIMD vector.
primitive Vec2(a:Type);
/// 4-lane SIMD vector.
primitive Vec4(a:Type);
/// 8-lane SIMD vector.
primitive Vec8(a:Type);
/// 16-lane SIMD vector.
primitive Vec16(a:Type);
