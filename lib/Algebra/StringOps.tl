/// Basic string operations: concatenation and length.
///
/// @see Algebra.StringExt, Numeric.String
module Algebra.StringOps;

/// String concatenation and length algebra.
algebra StringOps(a:Type) = {
    /// Concatenate two strings.
    /// @param x Left string
    /// @param y Right string
    /// @returns The concatenation of x and y
    /// @example concat("hello", " world") => "hello world"
    function concat(x:a, y:a) : a,

    /// String length.
    /// @param x A string
    /// @returns The number of characters
    /// @example length("hello") => 5
    function length(x:a) : Int,

    law concat_associativity(x:a, y:a, z:a) = concat(concat(x, y), z) === concat(x, concat(y, z))
};
