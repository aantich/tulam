/// Str string type: UTF-8 encoded immutable strings in pure tulam.
///
/// Includes the Str record type, UTF-8 codec, algebra instances
/// (Eq, Ord, Semigroup, Monoid, Show), StringLike implementation,
/// and the FromString algebra.
module Str;
import Core;
import Algebra;
import Morphism;
import Collection;

// ============================================================================
// Str type
// ============================================================================

/// UTF-8 encoded immutable string.
type Str = bytes: Array(Byte) * byteLen: Int;

/// Empty string constant.
function strEmpty() : Str = Str([], 0);

/// Check if a Str is empty.
function strIsEmpty(s:Str) : Bool = s.byteLen == 0;

// ============================================================================
// UTF-8 codec
// ============================================================================

/// Convert an Int to a Byte (truncating to low 8 bits).
function intToByte(n:Int) : Byte = intrinsic;

/// Convert a Byte to an Int (widening).
function byteToInt(b:Byte) : Int = intrinsic;

/// Encode a Unicode code point (as Int) to 1-4 UTF-8 bytes.
function encodeCodePoint(cp:Int) : Array(Byte) =
    if cp < 0x80 then
        [intToByte(cp)]
    else if cp < 0x800 then
        [intToByte(0xC0 .|. shiftR(cp, 6)),
         intToByte(0x80 .|. (cp .&. 0x3F))]
    else if cp < 0x10000 then
        [intToByte(0xE0 .|. shiftR(cp, 12)),
         intToByte(0x80 .|. (shiftR(cp, 6) .&. 0x3F)),
         intToByte(0x80 .|. (cp .&. 0x3F))]
    else
        [intToByte(0xF0 .|. shiftR(cp, 18)),
         intToByte(0x80 .|. (shiftR(cp, 12) .&. 0x3F)),
         intToByte(0x80 .|. (shiftR(cp, 6) .&. 0x3F)),
         intToByte(0x80 .|. (cp .&. 0x3F))];

/// Determine the byte length of a UTF-8 sequence from its lead byte.
function utf8SeqLen(leadByte:Byte) : Int =
    let b = byteToInt(leadByte) in
    if b < 0x80 then 1
    else if b < 0xC0 then 0
    else if b < 0xE0 then 2
    else if b < 0xF0 then 3
    else if b < 0xF8 then 4
    else 0;

/// Check if a byte is a UTF-8 continuation byte (10xxxxxx).
function isContinuation(b:Byte) : Bool =
    let i = byteToInt(b) in
    (i .&. 0xC0) == 0x80;

/// Decode one code point starting at byte offset i.
function decodeAt(bytes:Array(Byte), i:Int) : Maybe(Pair(Int, Int)) =
    if i >= size(bytes) then Nothing
    else
        let b0 = byteToInt(index(bytes, i)) in
        let seqLen = utf8SeqLen(intToByte(b0)) in
        if seqLen == 0 then Nothing
        else if seqLen == 1 then Just(Pair(b0, i + 1))
        else if i + seqLen > size(bytes) then Nothing
        else if seqLen == 2 then
            let b1 = byteToInt(index(bytes, i + 1)) in
            if (b1 .&. 0xC0) != 0x80 then Nothing
            else
                let cp = shiftL(b0 .&. 0x1F, 6) .|. (b1 .&. 0x3F) in
                Just(Pair(cp, i + 2))
        else if seqLen == 3 then
            let b1 = byteToInt(index(bytes, i + 1)) in
            let b2 = byteToInt(index(bytes, i + 2)) in
            if (b1 .&. 0xC0) != 0x80 then Nothing
            else if (b2 .&. 0xC0) != 0x80 then Nothing
            else
                let cp = shiftL(b0 .&. 0x0F, 12) .|. shiftL(b1 .&. 0x3F, 6) .|. (b2 .&. 0x3F) in
                Just(Pair(cp, i + 3))
        else
            let b1 = byteToInt(index(bytes, i + 1)) in
            let b2 = byteToInt(index(bytes, i + 2)) in
            let b3 = byteToInt(index(bytes, i + 3)) in
            if (b1 .&. 0xC0) != 0x80 then Nothing
            else if (b2 .&. 0xC0) != 0x80 then Nothing
            else if (b3 .&. 0xC0) != 0x80 then Nothing
            else
                let cp = shiftL(b0 .&. 0x07, 18) .|. shiftL(b1 .&. 0x3F, 12) .|. shiftL(b2 .&. 0x3F, 6) .|. (b3 .&. 0x3F) in
                Just(Pair(cp, i + 4));

/// Count the number of code points in a UTF-8 byte array.
function countCodePoints(bytes:Array(Byte)) : Int =
    countCodePointsFrom(bytes, 0, 0);

function countCodePointsFrom(bytes:Array(Byte), i:Int, count:Int) : Int =
    if i >= size(bytes) then count
    else
        let seqLen = utf8SeqLen(index(bytes, i)) in
        if seqLen == 0 then count
        else countCodePointsFrom(bytes, i + seqLen, count + 1);

/// Get the byte offset of the n-th code point (0-indexed).
function nthByteOffset(bytes:Array(Byte), n:Int) : Maybe(Int) =
    nthByteOffsetFrom(bytes, 0, n);

function nthByteOffsetFrom(bytes:Array(Byte), i:Int, n:Int) : Maybe(Int) =
    if n == 0 then
        if i <= size(bytes) then Just(i) else Nothing
    else if i >= size(bytes) then Nothing
    else
        let seqLen = utf8SeqLen(index(bytes, i)) in
        if seqLen == 0 then Nothing
        else nthByteOffsetFrom(bytes, i + seqLen, n - 1);

/// Validate that a byte array is well-formed UTF-8.
function isValidUtf8(bytes:Array(Byte)) : Bool =
    isValidUtf8From(bytes, 0);

function isValidUtf8From(bytes:Array(Byte), i:Int) : Bool =
    if i >= size(bytes) then True
    else isValidUtf8Check(decodeAt(bytes, i), bytes);

function isValidUtf8Check(result:Maybe(Pair(Int, Int)), bytes:Array(Byte)) : Bool = match
    | Nothing, bytes -> False
    | Just(Pair(cp, next)), bytes -> isValidUtf8From(bytes, next);

// ============================================================================
// Str algebra instances
// ============================================================================

/// Helper: compare byte arrays element-by-element for equality.
function eqBytes(a:Array(Byte), b:Array(Byte), i:Int, len:Int) : Bool =
    if i >= len then True
    else if index(a, i) != index(b, i) then False
    else eqBytes(a, b, i + 1, len);

/// Helper: lexicographic byte array comparison.
function compareBytes(a:Array(Byte), b:Array(Byte), i:Int, minLen:Int) : Ordering =
    if i >= minLen then compare(size(a), size(b))
    else
        let ba = byteToInt(index(a, i)) in
        let bb = byteToInt(index(b, i)) in
        if ba < bb then LessThan
        else if ba > bb then GreaterThan
        else compareBytes(a, b, i + 1, minLen);

/// Helper: minimum of two Ints.
function minInt(a:Int, b:Int) : Int =
    if a <= b then a else b;

instance Eq(Str) = {
    function (==)(a:Str, b:Str) : Bool =
        if a.byteLen != b.byteLen then False
        else eqBytes(a.bytes, b.bytes, 0, a.byteLen);
    function (!=)(a:Str, b:Str) : Bool =
        if a.byteLen != b.byteLen then True
        else not(eqBytes(a.bytes, b.bytes, 0, a.byteLen))
};

instance Ord(Str) = {
    function compare(a:Str, b:Str) : Ordering =
        compareBytes(a.bytes, b.bytes, 0, minInt(a.byteLen, b.byteLen));
    function (<)(a:Str, b:Str) : Bool = match compare(a, b)
        | LessThan -> True
        | _ -> False;
    function (>)(a:Str, b:Str) : Bool = match compare(a, b)
        | GreaterThan -> True
        | _ -> False;
    function (<=)(a:Str, b:Str) : Bool = match compare(a, b)
        | GreaterThan -> False
        | _ -> True;
    function (>=)(a:Str, b:Str) : Bool = match compare(a, b)
        | LessThan -> False
        | _ -> True
};

instance Semigroup(Str) = {
    function combine(a:Str, b:Str) : Str =
        Str(combine(a.bytes, b.bytes), a.byteLen + b.byteLen)
};

instance Monoid(Str) = {
    function combine(a:Str, b:Str) : Str =
        Str(combine(a.bytes, b.bytes), a.byteLen + b.byteLen);
    value empty : Str = Str([], 0)
};

/// Helper: decode all bytes to build a display string.
function strToDisplayString(bytes:Array(Byte), i:Int, acc:String) : String =
    if i >= size(bytes) then acc
    else strToDisplayStep(decodeAt(bytes, i), bytes, i, acc);

function strToDisplayStep(result:Maybe(Pair(Int, Int)), bytes:Array(Byte), i:Int, acc:String) : String = match
    | Nothing, bytes, i, acc -> concat(acc, "?")
    | Just(Pair(cp, next)), bytes, i, acc ->
        let ch = toEnum(cp) in
        strToDisplayString(bytes, next, concat(acc, show(ch)));

instance Show(Str) = {
    function show(s:Str) : String = strToDisplayString(s.bytes, 0, "")
};

// ============================================================================
// StringLike(Str) instance
// ============================================================================

/// Helper: search for needle bytes in haystack starting at byte offset.
function findBytes(haystack:Array(Byte), hLen:Int, needle:Array(Byte), nLen:Int, i:Int) : Int =
    if i + nLen > hLen then 0 - 1
    else if matchBytesAt(haystack, needle, i, 0, nLen) then i
    else findBytes(haystack, hLen, needle, nLen, i + 1);

/// Helper: check if needle matches at position pos in haystack.
function matchBytesAt(haystack:Array(Byte), needle:Array(Byte), pos:Int, j:Int, nLen:Int) : Bool =
    if j >= nLen then True
    else if index(haystack, pos + j) != index(needle, j) then False
    else matchBytesAt(haystack, needle, pos, j + 1, nLen);

/// Helper: convert a decoded Int pair to a Char pair.
function decodeIntToChar(result:Maybe(Pair(Int, Int))) : Maybe(Pair(Char, Int)) = match
    | Nothing -> Nothing
    | Just(Pair(cp, next)) -> Just(Pair(toEnum(cp), next));

instance StringLike(Str) = {
    function byteLength(s:Str) : Int = s.byteLen;

    function strSlice(s:Str, start:Int, end:Int) : Str =
        let sliced = slice(s.bytes, start, end) in
        Str(sliced, end - start);

    function strDecodeAt(s:Str, byteOffset:Int) : Maybe(Pair(Char, Int)) =
        decodeIntToChar(decodeAt(s.bytes, byteOffset));

    function strIndexOf(haystack:Str, needle:Str) : Int =
        if needle.byteLen == 0 then 0
        else findBytes(haystack.bytes, haystack.byteLen, needle.bytes, needle.byteLen, 0);

    function strFromChar(c:Char) : Str =
        let bs = encodeCodePoint(fromEnum(c)) in
        Str(bs, size(bs))
};

// ============================================================================
// FromString algebra
// ============================================================================

/// Algebra for constructing values from UTF-8 byte arrays.
algebra FromString(a:Type) = {
    function fromStringLiteral(bytes:Array(Byte)) : a
};

/// Str can be constructed directly from byte arrays.
instance FromString(Str) = {
    function fromStringLiteral(bytes:Array(Byte)) : Str =
        Str(bytes, size(bytes))
};
