/// FromString algebra for string literal desugaring.
///
/// When the compiler desugars string literals, "hello" becomes
/// fromStringLiteral([0x68, 0x65, 0x6C, 0x6C, 0x6F]).
/// Any type can implement FromString to accept string literals.
///
/// @see String.Str, Algebra.StringLike
module String.FromString;
import String.Str;

/// Algebra for constructing values from UTF-8 byte arrays.
algebra FromString(a:Type) = {
    /// Construct a value from a UTF-8 encoded byte array.
    /// @param bytes The UTF-8 bytes of the string literal
    /// @returns The constructed value
    function fromStringLiteral(bytes:Array(Byte)) : a
};

/// Str can be constructed directly from byte arrays.
instance FromString(Str) = {
    function fromStringLiteral(bytes:Array(Byte)) : Str =
        Str(bytes, length(bytes))
};
