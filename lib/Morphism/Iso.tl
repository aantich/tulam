/// Isomorphism — lossless reversible conversion between two types.
///
/// Every value of a maps to exactly one value of b, and vice versa.
/// Both convert and unconvert are total functions that form a round-trip.
///
/// @see Morphism.Convertible, Morphism.Embed
module Morphism.Iso;
import Morphism.Convertible;

/// Lossless bidirectional type conversion.
morphism Iso(a:Type, b:Type) extends Convertible(a, b) = {
    /// The inverse of convert — maps b back to a.
    /// @param x A value of type b
    /// @returns The corresponding value of type a
    function unconvert(x:b) : a,

    law roundtrip1(x:a) = unconvert(convert(x)) === x,
    law roundtrip2(y:b) = convert(unconvert(y)) === y
};
