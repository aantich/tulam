/// Bitwise operations for integer types.
///
/// Provides bitwise AND, OR, XOR, complement, and shifts.
///
/// @see Numeric.Int, Numeric.UInt
module Algebra.Bits;

/// Bitwise operations algebra.
algebra Bits(a:Type) = {
    /// Bitwise AND.
    function (.&.)(x:a, y:a) : a,
    /// Bitwise OR.
    function (.|.)(x:a, y:a) : a,
    /// Bitwise XOR (exclusive or).
    function xor(x:a, y:a) : a,
    /// Bitwise complement (flip all bits).
    function complement(x:a) : a,
    /// Left shift by y positions.
    function shiftL(x:a, y:a) : a,
    /// Right shift by y positions.
    function shiftR(x:a, y:a) : a,
    /// Number of bits in the type.
    value bitSize : a,

    law double_complement(x:a) = complement(complement(x)) === x,
    law and_idempotent(x:a)   = x .&. x === x,
    law or_idempotent(x:a)    = x .|. x === x,
    law xor_self(x:a, y:a)    = xor(x, x) === xor(y, y),
    law and_commutativity(x:a, y:a) = x .&. y === y .&. x,
    law or_commutativity(x:a, y:a)  = x .|. y === y .|. x,
    law xor_commutativity(x:a, y:a) = xor(x, y) === xor(y, x)
};
