/// String algebras: StringOps, StringExt, StringLike.
///
/// Extracted from Algebra.tl. These define the behavioral contracts
/// for string-like types. Instances are in Instances/Numeric.tl (String)
/// and Str.tl (Str).
module String;
import Core;
import Algebra;
import Reflection;

// ============================================================================
// StringOps
// ============================================================================

/// String concatenation and length algebra.
algebra StringOps(a:Type) = {
    function concat(x:a, y:a) : a;
    function length(x:a) : Int;

    law concat_associativity(x:a, y:a, z:a) = concat(concat(x, y), z) === concat(x, concat(y, z))
};

// ============================================================================
// StringExt
// ============================================================================

/// Extended string manipulation algebra.
algebra StringExt(a:Type) = {
    function charAt(s:a, i:Int) : Char;
    function substring(s:a, start:Int, end:Int) : a;
    function indexOf(s:a, needle:a) : Int;
    function trim(s:a) : a;
    function toUpper(s:a) : a;
    function toLower(s:a) : a;
    function startsWith(s:a, prefix:a) : Bool;
    function endsWith(s:a, suffix:a) : Bool;
    function replace(s:a, old:a, new:a) : a;
    function parseInt(s:a) : Maybe(Int);
    function parseFloat(s:a) : Maybe(Float64);
    /// Split a string by a delimiter into a list of substrings.
    function split(s:a, delim:a) : List(a);
    /// Join a list of strings with a separator.
    function join(parts:List(a), sep:a) : a;
    /// Convert a single character to a string.
    function fromChar(c:Char) : a
};

// ============================================================================
// Character helpers
// ============================================================================

/// Check if a code point is ASCII whitespace.
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

// ============================================================================
// StringLike
// ============================================================================

/// String-specific operations algebra.
/// Types implementing StringLike should also implement Eq, Ord, and Monoid.
algebra StringLike(s:Type) = {
    // === Core methods (required, no body) ===

    function byteLength(str:s) : Int;
    function strSlice(str:s, start:Int, end:Int) : s;
    function strDecodeAt(str:s, byteOffset:Int) : Maybe(Pair(Char, Int));
    function strIndexOf(haystack:s, needle:s) : Int;
    function strFromChar(c:Char) : s;

    // === High-level operations (defaulted) ===

    function charCount(str:s) : Int = charCountFrom(str, 0, 0);

    function charCountFrom(str:s, byteOff:Int, count:Int) : Int =
        charCountStep(strDecodeAt(str, byteOff), count, str);

    function charCountStep(r:Maybe(Pair(Char, Int)), count:Int, str:s) : Int = match
        | Nothing, count, str -> count
        | Just(Pair(ch, nextOff)), count, str -> charCountFrom(str, nextOff, count + 1);

    function nthChar(str:s, n:Int) : Maybe(Char) = nthCharHelper(str, 0, n);

    function nthCharHelper(str:s, byteOff:Int, remaining:Int) : Maybe(Char) =
        nthCharStep(strDecodeAt(str, byteOff), remaining, str);

    function nthCharStep(r:Maybe(Pair(Char, Int)), remaining:Int, str:s) : Maybe(Char) = match
        | Nothing, remaining, str -> Nothing
        | Just(Pair(ch, nextOff)), remaining, str ->
            if remaining == 0 then Just(ch)
            else nthCharHelper(str, nextOff, remaining - 1);

    function toChars(str:s) : List(Char) = toCharsFrom(str, 0);

    function toCharsFrom(str:s, byteOff:Int) : List(Char) =
        toCharsStep(strDecodeAt(str, byteOff), str);

    function toCharsStep(r:Maybe(Pair(Char, Int)), str:s) : List(Char) = match
        | Nothing, str -> Nil
        | Just(Pair(ch, nextOff)), str -> Cons(ch, toCharsFrom(str, nextOff));

    function strStartsWith(str:s, prefix:s) : Bool =
        let pLen = byteLength(prefix) in
        if pLen > byteLength(str) then False
        else strSlice(str, 0, pLen) == prefix;

    function strEndsWith(str:s, suffix:s) : Bool =
        let { sLen = byteLength(suffix); tLen = byteLength(str) } in
        if sLen > tLen then False
        else strSlice(str, tLen - sLen, tLen) == suffix;

    function strTrim(str:s) : s =
        let { startOff = trimLeading(str, 0); endOff = trimTrailing(str, byteLength(str)) } in
        if startOff >= endOff then strSlice(str, 0, 0)
        else strSlice(str, startOff, endOff);

    function trimLeading(str:s, byteOff:Int) : Int =
        trimLeadingStep(strDecodeAt(str, byteOff), byteOff, str);

    function trimLeadingStep(r:Maybe(Pair(Char, Int)), byteOff:Int, str:s) : Int = match
        | Nothing, byteOff, str -> byteOff
        | Just(Pair(ch, nextOff)), byteOff, str ->
            if isWhitespace(ch) then trimLeading(str, nextOff)
            else byteOff;

    function trimTrailing(str:s, endOff:Int) : Int =
        if endOff <= 0 then 0
        else
            let lastCharStart = endOff - 1 in
            trimTrailingStep(strDecodeAt(str, lastCharStart), lastCharStart, endOff, str);

    function trimTrailingStep(r:Maybe(Pair(Char, Int)), lastCharStart:Int, endOff:Int, str:s) : Int = match
        | Nothing, lastCharStart, endOff, str -> endOff
        | Just(Pair(ch, _)), lastCharStart, endOff, str ->
            if isWhitespace(ch) then trimTrailing(str, lastCharStart)
            else endOff;

    function strToUpper(str:s) : s = mapChars(str, charToUpper, 0);
    function strToLower(str:s) : s = mapChars(str, charToLower, 0);

    function mapChars(str:s, f:Char -> Char, byteOff:Int) : s =
        mapCharsStep(strDecodeAt(str, byteOff), str, f, byteOff);

    function mapCharsStep(r:Maybe(Pair(Char, Int)), str:s, f:Char -> Char, byteOff:Int) : s = match
        | Nothing, str, f, byteOff -> strSlice(str, 0, 0)
        | Just(Pair(ch, nextOff)), str, f, byteOff ->
            let { mapped = strFromChar(f(ch)); rest = mapChars(str, f, nextOff) } in
            combine(mapped, rest);

    function strReplace(str:s, old:s, new:s) : s =
        let oldLen = byteLength(old) in
        if oldLen == 0 then str
        else strReplaceFrom(str, old, new, oldLen, 0);

    function strReplaceFrom(str:s, old:s, new:s, oldLen:Int, start:Int) : s =
        let { remaining = strSlice(str, start, byteLength(str)); pos = strIndexOf(remaining, old) } in
        if pos < 0 then remaining
        else
            let { before = strSlice(remaining, 0, pos); after = strReplaceFrom(str, old, new, oldLen, start + pos + oldLen) } in
            combine(combine(before, new), after);

    function strSplit(str:s, delim:s) : List(s) =
        let dLen = byteLength(delim) in
        if dLen == 0 then Cons(str, Nil)
        else strSplitFrom(str, delim, dLen, 0);

    function strSplitFrom(str:s, delim:s, dLen:Int, start:Int) : List(s) =
        let { remaining = strSlice(str, start, byteLength(str)); pos = strIndexOf(remaining, delim) } in
        if pos < 0 then Cons(remaining, Nil)
        else
            let { before = strSlice(remaining, 0, pos); rest = strSplitFrom(str, delim, dLen, start + pos + dLen) } in
            Cons(before, rest);

    function strJoin(parts:List(s), sep:s) : s = match
        | Nil, sep -> strSlice(sep, 0, 0)
        | Cons(h, Nil), sep -> h
        | Cons(h, t), sep -> combine(combine(h, sep), strJoin(t, sep))
};
