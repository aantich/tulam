/// Representation mapping: Char as Int.
///
/// Allows converting between Unicode code points and their integer values.
/// Uses existing Enum(Char) intrinsics for the conversion.
///
/// @see Numeric.Char, Algebra.Enum
module Repr.Char;
import Algebra.Enum;

/// Map Char to Int via Unicode code point value.
/// 'A' <-> 65, 'a' <-> 97, etc.
repr Char as Int where {
    function toRepr(c:Char) : Int = fromEnum(c),
    function fromRepr(i:Int) : Char = toEnum(i)
};
