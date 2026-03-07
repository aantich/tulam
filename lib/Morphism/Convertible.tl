/// Convertible — one-way type conversion.
///
/// The simplest morphism: convert a value of type a to type b.
/// May lose information (unlike Iso which is lossless).
///
/// @see Morphism.Iso, Morphism.Embed
module Morphism.Convertible;

/// One-way type conversion morphism.
morphism Convertible(a:Type, b:Type) = {
    /// Convert a value from type a to type b.
    /// @param x The value to convert
    /// @returns The converted value
    function convert(x:a) : b
};
