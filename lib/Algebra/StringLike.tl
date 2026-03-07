/// Universal string contract.
///
/// StringLike defines string-specific operations that no existing algebra covers.
/// String types also implement Eq, Ord, and Monoid separately for
/// equality, comparison, and concatenation.
///
/// Core methods (5 required): byteLength, strSlice, strDecodeAt, strIndexOf, strFromChar.
/// All other methods have default implementations built on the core methods.
/// Native targets (.NET, JS, C++) can override any method with native calls.
///
/// @see Algebra.Eq, Algebra.Ord, Algebra.Semigroup
module Algebra.StringLike;
import Core.Bool;
import Core.Maybe;
import Core.Pair;
import Core.List;
import Algebra.Semigroup;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Enum;

/// Check if a code point is ASCII whitespace (space, tab, newline, carriage return).
function isWhitespace(c:Char) : Bool =
    let cp = fromEnum(c) in
    if cp == 32 then True
    else if cp == 9 then True
    else if cp == 10 then True
    else if cp == 13 then True
    else False;

/// Check if a char is an ASCII uppercase letter.
function isUpperCase(c:Char) : Bool =
    let cp = fromEnum(c) in
    if cp >= 65 then cp <= 90 else False;

/// Check if a char is an ASCII lowercase letter.
function isLowerCase(c:Char) : Bool =
    let cp = fromEnum(c) in
    if cp >= 97 then cp <= 122 else False;

/// Convert a single char to uppercase (ASCII only).
function charToUpper(c:Char) : Char =
    if isLowerCase(c) then toEnum(fromEnum(c) - 32)
    else c;

/// Convert a single char to lowercase (ASCII only).
function charToLower(c:Char) : Char =
    if isUpperCase(c) then toEnum(fromEnum(c) + 32)
    else c;

/// String-specific operations algebra.
/// Types implementing StringLike should also implement Eq, Ord, and Monoid.
algebra StringLike(s:Type) = {
    // === Core methods (required, no body) ===

    /// Byte length (O(1) for UTF-8 strings).
    function byteLength(str:s) : Int,

    /// Extract substring by byte range [start, end).
    function strSlice(str:s, start:Int, end:Int) : s,

    /// Decode one code point at byte offset.
    /// Returns Just(Pair(char, nextByteOffset)) or Nothing if past end.
    function strDecodeAt(str:s, byteOffset:Int) : Maybe(Pair(Char, Int)),

    /// Find byte offset of first occurrence of needle. Returns -1 if not found.
    function strIndexOf(haystack:s, needle:s) : Int,

    /// Construct a string from a single character.
    function strFromChar(c:Char) : s,

    // === High-level operations (defaulted — override for native perf) ===

    /// Count Unicode code points (O(n) — walks bytes).
    function charCount(str:s) : Int = charCountFrom(str, 0, 0),

    /// Helper: count chars from byte offset.
    function charCountFrom(str:s, byteOff:Int, count:Int) : Int =
        charCountStep(strDecodeAt(str, byteOff), count, str),

    /// Helper: process one step of charCount iteration.
    function charCountStep(r:Maybe(Pair(Char, Int)), count:Int, str:s) : Int = match
        | Nothing, count, str -> count
        | Just(Pair(ch, nextOff)), count, str -> charCountFrom(str, nextOff, count + 1),

    /// Get the n-th Unicode code point (O(n) — walks bytes from start).
    function nthChar(str:s, n:Int) : Maybe(Char) = nthCharHelper(str, 0, n),

    /// Helper: walk byte offsets to find the n-th code point.
    function nthCharHelper(str:s, byteOff:Int, remaining:Int) : Maybe(Char) =
        nthCharStep(strDecodeAt(str, byteOff), remaining, str),

    /// Helper: process one step of nthChar iteration.
    function nthCharStep(r:Maybe(Pair(Char, Int)), remaining:Int, str:s) : Maybe(Char) = match
        | Nothing, remaining, str -> Nothing
        | Just(Pair(ch, nextOff)), remaining, str ->
            if remaining == 0 then Just(ch)
            else nthCharHelper(str, nextOff, remaining - 1),

    /// Iterate all code points as a List.
    function toChars(str:s) : List(Char) = toCharsFrom(str, 0),

    /// Helper: collect chars from a byte offset.
    function toCharsFrom(str:s, byteOff:Int) : List(Char) =
        toCharsStep(strDecodeAt(str, byteOff), str),

    /// Helper: process one step of toChars iteration.
    function toCharsStep(r:Maybe(Pair(Char, Int)), str:s) : List(Char) = match
        | Nothing, str -> Nil
        | Just(Pair(ch, nextOff)), str -> Cons(ch, toCharsFrom(str, nextOff)),

    /// Check if string starts with the given prefix.
    function strStartsWith(str:s, prefix:s) : Bool =
        let pLen = byteLength(prefix) in
        if pLen > byteLength(str) then False
        else strSlice(str, 0, pLen) == prefix,

    /// Check if string ends with the given suffix.
    function strEndsWith(str:s, suffix:s) : Bool =
        let sLen = byteLength(suffix) in
        let tLen = byteLength(str) in
        if sLen > tLen then False
        else strSlice(str, tLen - sLen, tLen) == suffix,

    /// Trim whitespace from both ends of a string.
    function strTrim(str:s) : s =
        let startOff = trimLeading(str, 0) in
        let endOff = trimTrailing(str, byteLength(str)) in
        if startOff >= endOff then strSlice(str, 0, 0)
        else strSlice(str, startOff, endOff),

    /// Helper: find first non-whitespace byte offset from left.
    function trimLeading(str:s, byteOff:Int) : Int =
        trimLeadingStep(strDecodeAt(str, byteOff), byteOff, str),

    /// Helper step for trimLeading.
    function trimLeadingStep(r:Maybe(Pair(Char, Int)), byteOff:Int, str:s) : Int = match
        | Nothing, byteOff, str -> byteOff
        | Just(Pair(ch, nextOff)), byteOff, str ->
            if isWhitespace(ch) then trimLeading(str, nextOff)
            else byteOff,

    /// Helper: find last non-whitespace byte offset from right.
    function trimTrailing(str:s, endOff:Int) : Int =
        if endOff <= 0 then 0
        else
            let lastCharStart = endOff - 1 in
            trimTrailingStep(strDecodeAt(str, lastCharStart), lastCharStart, endOff, str),

    /// Helper step for trimTrailing.
    function trimTrailingStep(r:Maybe(Pair(Char, Int)), lastCharStart:Int, endOff:Int, str:s) : Int = match
        | Nothing, lastCharStart, endOff, str -> endOff
        | Just(Pair(ch, _)), lastCharStart, endOff, str ->
            if isWhitespace(ch) then trimTrailing(str, lastCharStart)
            else endOff,

    /// Convert string to uppercase (ASCII letters only).
    function strToUpper(str:s) : s = mapChars(str, charToUpper, 0),

    /// Convert string to lowercase (ASCII letters only).
    function strToLower(str:s) : s = mapChars(str, charToLower, 0),

    /// Map a function over all characters, rebuilding the string.
    function mapChars(str:s, f:Char -> Char, byteOff:Int) : s =
        mapCharsStep(strDecodeAt(str, byteOff), str, f, byteOff),

    /// Helper step for mapChars.
    function mapCharsStep(r:Maybe(Pair(Char, Int)), str:s, f:Char -> Char, byteOff:Int) : s = match
        | Nothing, str, f, byteOff -> strSlice(str, 0, 0)
        | Just(Pair(ch, nextOff)), str, f, byteOff ->
            let mapped = strFromChar(f(ch)) in
            let rest = mapChars(str, f, nextOff) in
            combine(mapped, rest),

    /// Replace all occurrences of old with new in a string.
    function strReplace(str:s, old:s, new:s) : s =
        let oldLen = byteLength(old) in
        if oldLen == 0 then str
        else strReplaceFrom(str, old, new, oldLen, 0),

    /// Helper: replace from a byte offset.
    function strReplaceFrom(str:s, old:s, new:s, oldLen:Int, start:Int) : s =
        let remaining = strSlice(str, start, byteLength(str)) in
        let pos = strIndexOf(remaining, old) in
        if pos < 0 then remaining
        else
            let before = strSlice(remaining, 0, pos) in
            let after = strReplaceFrom(str, old, new, oldLen, start + pos + oldLen) in
            combine(combine(before, new), after),

    /// Split a string by a delimiter.
    function strSplit(str:s, delim:s) : List(s) =
        let dLen = byteLength(delim) in
        if dLen == 0 then Cons(str, Nil)
        else strSplitFrom(str, delim, dLen, 0),

    /// Helper: split from a byte offset.
    function strSplitFrom(str:s, delim:s, dLen:Int, start:Int) : List(s) =
        let remaining = strSlice(str, start, byteLength(str)) in
        let pos = strIndexOf(remaining, delim) in
        if pos < 0 then Cons(remaining, Nil)
        else
            let before = strSlice(remaining, 0, pos) in
            let rest = strSplitFrom(str, delim, dLen, start + pos + dLen) in
            Cons(before, rest),

    /// Join a list of strings with a separator.
    function strJoin(parts:List(s), sep:s) : s = match
        | Nil, sep -> strSlice(sep, 0, 0)
        | Cons(h, Nil), sep -> h
        | Cons(h, t), sep -> combine(combine(h, sep), strJoin(t, sep))
};
