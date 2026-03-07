/// UTF-8 encoded immutable string type.
///
/// Str is the pure tulam string type, backed by an Array(Byte) of
/// UTF-8 encoded bytes and a cached byte length.
///
/// @see String.Utf8, Algebra.StringLike
module String.Str;

/// UTF-8 encoded immutable string.
/// @param bytes The UTF-8 encoded byte array
/// @param byteLen Cached byte length for O(1) access
record Str = { bytes: Array(Byte), byteLen: Int };

/// Empty string constant.
function strEmpty() : Str = Str([], 0);

/// Check if a Str is empty.
/// @param s The string to check
/// @returns True if the string has no bytes
function strIsEmpty(s:Str) : Bool = s.byteLen == 0;
