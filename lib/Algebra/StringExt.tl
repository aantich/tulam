/// Extended string operations: search, transform, parse.
///
/// @see Algebra.StringOps, Numeric.String
module Algebra.StringExt;
import Core.Maybe;

/// Extended string manipulation algebra.
algebra StringExt(a:Type) = {
    /// Character at index (0-based).
    /// @param s The string
    /// @param i Index position
    /// @returns The character at position i
    function charAt(s:a, i:Int) : Char,

    /// Extract a substring.
    /// @param s The string
    /// @param start Start index (inclusive)
    /// @param end End index (exclusive)
    /// @returns The substring from start to end
    function substring(s:a, start:Int, end:Int) : a,

    /// Find the first occurrence of a substring.
    /// @param s The string to search in
    /// @param needle The substring to find
    /// @returns Index of first occurrence, or -1 if not found
    function indexOf(s:a, needle:a) : Int,

    /// Remove leading and trailing whitespace.
    /// @param s The string
    /// @returns Trimmed string
    function trim(s:a) : a,

    /// Convert to uppercase.
    /// @param s The string
    /// @returns Uppercase version
    function toUpper(s:a) : a,

    /// Convert to lowercase.
    /// @param s The string
    /// @returns Lowercase version
    function toLower(s:a) : a,

    /// Check if string starts with a prefix.
    /// @param s The string
    /// @param prefix The prefix to check
    /// @returns True if s starts with prefix
    function startsWith(s:a, prefix:a) : Bool,

    /// Check if string ends with a suffix.
    /// @param s The string
    /// @param suffix The suffix to check
    /// @returns True if s ends with suffix
    function endsWith(s:a, suffix:a) : Bool,

    /// Replace all occurrences of a substring.
    /// @param s The string
    /// @param old Substring to replace
    /// @param new Replacement string
    /// @returns String with all occurrences replaced
    function replace(s:a, old:a, new:a) : a,

    /// Parse a string as an integer.
    /// @param s The string to parse
    /// @returns Just(n) if valid integer, Nothing otherwise
    function parseInt(s:a) : Maybe(Int),

    /// Parse a string as a floating-point number.
    /// @param s The string to parse
    /// @returns Just(f) if valid float, Nothing otherwise
    function parseFloat(s:a) : Maybe(Float64)
};
